(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open IdentifierSet
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

(* The variables in scope at a statement, by the name the body calls them. *)
type variables_in_scope = IdentifierSet.t

let binding_names (bindings: qbinding list): identifier list =
  List.map (fun (QBinding { rename; _ }) -> rename) bindings

let with_variables (variables: variables_in_scope) (names: identifier list): variables_in_scope =
  List.fold_left (fun variables name -> IdentifierSet.add name variables) variables names

(* Whether the call is one to `abort` of `Austral.Pervasive`. The combining
   pass qualified the name of what is called with the imports of the module,
   whatever variables were in scope. The type checker calls a variable of that
   name before any declaration (TypeCheckExpr.augment_call): where one is in
   scope, the call is to the function pointer it holds, which returns. *)
let calls_abort (variables: variables_in_scope) (name: qident): bool =
  is_abort name && not (IdentifierSet.mem (local_name name) variables)

let rec ends_in_return (variables: variables_in_scope) (stmt: astmt): bool =
  match stmt with
  | ASkip _ ->
     false
  | ALet (_, _, name, _, _, _, body) ->
     ends_in_return (with_variables variables [name]) body
  | ADestructure (_, _, bindings, _, body) ->
     ends_in_return (with_variables variables (binding_names bindings)) body
  | AAssign _ ->
     false
  | AIf (_, _, tb, fb) ->
     (ends_in_return variables tb) && (ends_in_return variables fb)
  | AWhen _ ->
     (* When the condition is false, control goes past the statement. *)
     false
  | ACase (_, _, whens) ->
     let l =
       List.map
         (fun (AbstractWhen (_, bindings, body)) -> ends_in_return (with_variables variables (binding_names bindings)) body)
         whens
     in
     List.for_all (fun x -> x) l
  | AWhile _ | AFor _ ->
     (* When the body runs zero times, control goes past the loop. *)
     false
  | ABorrow { rename; body; _ } ->
     ends_in_return (with_variables variables [rename]) body
  | ABlock (_, _, body) ->
     ends_in_return variables body
  | ADiscarding (_, e) ->
     (match e with
      | FunctionCall (name, _) ->
         calls_abort variables name
      | NilConstant | BoolConstant _ | IntConstant _ | FloatConstant _
      | StringConstant _ | Variable _ | ArithmeticExpression _ | Comparison _
      | Conjunction _ | Disjunction _ | Negation _ | IfExpression _ | Path _
      | RefPath _ | Embed _ | Deref _ | Typecast _ | SizeOf _ | BorrowExpr _
      | Reborrow _ ->
         false)
  | AReturn _ ->
     true

(* Whether a body with the given parameters ends in a return. *)
let body_ends_in_return (params: qparam list) (body: astmt): bool =
  let parameter_names = List.map (fun (QualifiedParameter (name, _)) -> name) params in
  ends_in_return (with_variables IdentifierSet.empty parameter_names) body

let check_method_ends_in_return (span: span) (CMethodDef (name, _, params, _, _, body)): unit =
  if body_ends_in_return params body then
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
  | CFunction (span, _, name, _, params, _, body, _, pragmas) ->
     (match pragmas with
      | [] | [Common.ForeignExportPragma _] ->
         (* A regular function, or one that is also exported: it has a body. *)
         if body_ends_in_return params body then
           ()
         else
           declaration_error (AtSpan span) (MissingReturn { name; declaration = FunctionDeclaration })
      | [Common.ForeignImportPragma _] ->
         (* An imported function has no body. *)
         ()
      | [Common.UnsafeModulePragma] | _ :: _ :: _ ->
         (* Not the pragmas of a function. The type checker rejects them. *)
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
