(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Imports
open Cst
module Ast = Stages.Ast
open Ast
open Escape
open Qualifier
open Span
open Error

let rec abs_stmt (place: place) im stmt =
  match stmt with
  | CSkip span ->
     ASkip span
  | CAssign (span, lvalue, value) ->
     AAssign (span, abs_lvalue place im lvalue, abs_expr place im value)
  | CIf (span, c, t, f) ->
     let c = abs_expr place im c
     and t = abs_stmt place im t
     and f = abs_stmt place im f
     in
     (match f with
      | ASkip _ ->
         (* If the else branch is just a skip statement, we rewrite this into a
            `when` (i.e. an `if` without an `else`). This is because in the pass
            where we check all functions have a return value we need else-less
            whens. *)
         AWhen (span, c, t)
      | ALet _ | ADestructure _ | AAssign _ | AIf _ | AWhen _
      | ACase _ | AWhile _ | AFor _ | ABorrow _ | ABlock _ | ADiscarding _ | AReturn _ ->
         AIf (span, c, t, f))
  | CCase (span, e, cases) ->
     ACase (span, abs_expr place im e, List.map (abs_when place im) cases)
  | CWhile (span, c, b) ->
     AWhile (span, abs_expr place im c, abs_stmt place im b)
  | CFor (span, n, i, f, b) ->
     AFor {
         span = span;
         name = n;
         initial = abs_expr place im i;
         final = abs_expr place im f;
         body = abs_stmt place im b
       }
  | CBorrow { span; original; rename; region; body; mode; _ } ->
     ABorrow { span; original; rename; region; body=abs_stmt place im body; mode }
  | CBlock (_, l) ->
     let_reshape place im l
  | CDiscarding (span, e) ->
     ADiscarding (span, abs_expr place im e)
  | CReturn (span, e) ->
     AReturn (span, abs_expr place im e)

and abs_expr place im expr =
  match expr with
  | CNilConstant _ -> NilConstant
  | CBoolConstant (_, b) -> BoolConstant b
  | CIntConstant (_, i) -> IntConstant i
  | CFloatConstant (_, f) -> FloatConstant f
  | CStringConstant (_, s) -> StringConstant (escape_string place s)
  | CVariable (_, i) -> Variable (qualify_identifier im i)
  | CFuncall (_, name, args) ->
     FunctionCall (qualify_identifier im name, abs_arglist place im args)
  | CArith (_, op, lhs, rhs) ->
     let e: aexpr = ArithmeticExpression (op, abs_expr place im lhs, abs_expr place im rhs) in
     LiteralTyping.fold_integer_arithmetic e
  | CComparison (_, op, lhs, rhs) ->
     Comparison (op, abs_expr place im lhs, abs_expr place im rhs)
  | CConjunction (_, lhs, rhs) ->
     Conjunction (abs_expr place im lhs, abs_expr place im rhs)
  | CDisjunction (_, lhs, rhs) ->
     Disjunction (abs_expr place im lhs, abs_expr place im rhs)
  | CNegation (_, e) ->
     Negation (abs_expr place im e)
  | CIfExpression (_, c, t, f) ->
     IfExpression (abs_expr place im c, abs_expr place im t, abs_expr place im f)
  | CPath (_, head, es) ->
     Path (qualify_identifier im head, List.map (abs_path_elem place im) es)
  | CRefPath (_, head, es) ->
     RefPath (qualify_identifier im head, List.map (abs_path_elem place im) es)
  | CEmbed (_, ty, expr, args) ->
     Embed (qualify_typespec im ty, expr, List.map (abs_expr place im) args)
  | CDeref (_, e) ->
     Deref (abs_expr place im e)
  | CTypecast (_, e, ty) ->
     Typecast (abs_expr place im e, qualify_typespec im ty)
  | CSizeOf (_, ty) ->
     SizeOf (qualify_typespec im ty)
  | CBorrowExpr (_, mode, var) ->
     BorrowExpr (mode, qualify_identifier im var)
  | CReborrow (_, name) ->
     Reborrow (qualify_identifier im name)

and abs_when place im (ConcreteWhen (name, bindings, body)) =
  AbstractWhen (name,
                List.map (fun (ConcreteBinding { name; ty; rename; }) -> QBinding { name; ty = qualify_typespec im ty; rename }) bindings,
                abs_stmt place im body)

and abs_arglist place im args =
  match args with
  | ConcretePositionalArgs l ->
     Positional (List.map (abs_expr place im) l)
  | ConcreteNamedArgs l ->
     Named (List.map (fun (n, v) -> (n, abs_expr place im v)) l)

and abs_lvalue place im (ConcreteLvalue (_, head, es)) =
  LValue (qualify_identifier im head, List.map (abs_path_elem place im) es)

and abs_path_elem place im elem =
  match elem with
  | CSlotAccessor i ->
     SlotAccessor i
  | CPointerSlotAccessor i ->
     PointerSlotAccessor i
  | CArrayIndex ie ->
     ArrayIndex (abs_expr place im ie)

(* Given a list of statements, find the first let statement, if any, and put the
   remainder of the list under its body. Then call let_reshape on that
   remainder. *)
and let_reshape (place: place) (im: import_map) (l: concrete_block_elem list): astmt =
  match l with
  | first::rest ->
     (match first with
      | CLet (span, mut, n, t, v) ->
         ALet (span, mut, n, qualify_typespec im t, qualify_identifier im n, abs_expr place im v, let_reshape place im rest)
      | CDestructure (span, mut, bs, e) ->
         let bs' = List.map (fun (ConcreteBinding {name; ty; rename; }) -> QBinding { name; ty = qualify_typespec im ty; rename; }) bs
         and e' = abs_expr place im e
         and b = let_reshape place im rest
         in
         ADestructure (span, mut, bs', e', b)
      | CStatement s ->
         (if rest = [] then
            abs_stmt place im s
          else
            ABlock (empty_span, abs_stmt place im s, let_reshape place im rest)))
  | [] ->
     ASkip empty_span
