(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Stages.Ast
open Stages.Combined
open Error
open Span

let is_abort (name: qident): bool =
  let mn: module_name = source_module_name name
  and orig: identifier = original_name name
  in
  (((mod_name_string mn) = BuiltInNames.pervasive_module)
   && ((ident_string orig) = BuiltInNames.abort_name))

let rec ends_in_return (stmt: astmt): bool =
  match stmt with
  | ASkip _ ->
     false
  | ALet (_, _, _, _, _, _, body) ->
     ends_in_return body
  | ADestructure (_, _, _, _, body) ->
     ends_in_return body
  | AAssign _ ->
     false
  | AIf (_, _, tb, fb) ->
     (ends_in_return tb) && (ends_in_return fb)
  | AWhen (_, _, b) ->
     ends_in_return b
  | ACase (_, _, whens) ->
     let l = List.map (fun (AbstractWhen (_, _, body)) -> ends_in_return body) whens in
     List.for_all (fun x -> x) l
  | AWhile (_, _, body) ->
     ends_in_return body
  | AFor { body; _ } ->
     ends_in_return body
  | ABorrow { body; _ } ->
     ends_in_return body
  | ABlock (_, _, body) ->
     ends_in_return body
  | ADiscarding (_, e) ->
     (match e with
      | FunctionCall (name, _) ->
         is_abort name
      | NilConstant | BoolConstant _ | IntConstant _ | FloatConstant _
      | StringConstant _ | Variable _ | ArithmeticExpression _ | Comparison _
      | Conjunction _ | Disjunction _ | Negation _ | IfExpression _ | Path _
      | RefPath _ | Embed _ | Deref _ | Typecast _ | SizeOf _ | BorrowExpr _
      | Reborrow _ ->
         false)
  | AReturn _ ->
     true

let check_method_ends_in_return (span: span) (CMethodDef (name, _, _, _, _, body)): unit =
  if ends_in_return body then
    ()
  else
    declaration_error (AtSpan span) (MissingReturn { name; declaration = MethodDeclaration })

let check_decl_ends_in_return (decl: combined_definition): unit =
  match decl with
  | CConstant _ ->
     ()
  | CRecord _ ->
     ()
  | CUnion _ ->
     ()
  | CFunction (span, _, name, _, _, _, body, _, pragmas) ->
     (match pragmas with
      | [] ->
         (* No pragmas: regular function. *)
         if ends_in_return body then
           ()
         else
           declaration_error (AtSpan span) (MissingReturn { name; declaration = FunctionDeclaration })
      | [Common.ForeignImportPragma _] | [Common.ForeignExportPragma _] | [Common.UnsafeModulePragma] | _ :: _ :: _ ->
         (* Yes pragmas: foreign function, ignore the body. *)
         ())
  | CTypeclass _ ->
     ()
  | CInstance (span, _, _, _, _, _, methods, _) ->
     let _ = List.map (check_method_ends_in_return span) methods in
     ()

type returning_module = ReturningModule of combined_module

let check_ends_in_return (md: combined_module): returning_module Outcome.t =
  let (CombinedModule { decls; _ }) = md in
  Outcome.map (fun (_: unit list) -> ReturningModule md)
    (Outcome.traverse (fun decl -> Outcome.catch (fun () -> check_decl_ends_in_return decl)) decls)

let as_combined_module (ReturningModule md): combined_module =
  md
