(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Stages.Tast

let rec depends_on_evaluation_order (write_borrowed_variables: identifier list) (expr: texpr): bool =
  let depends: texpr -> bool = depends_on_evaluation_order write_borrowed_variables in
  match expr with
  | TNilConstant | TBoolConstant _ | TIntConstant _ | TFloatConstant _ | TStringConstant _
  | TConstVar _ | TFunVar _ | TTemporary _ | TSizeOf _ ->
     false
  | TParamVar (name, _) | TLocalVar (name, _) ->
     List.exists (equal_identifier name) write_borrowed_variables
  | TFuncall _ | TMethodCall _ | TVarMethodCall _ | TFptrCall _ | TEmbed _
  | TArithmeticNegation _ | TArrayIndex _ | TSpanIndex _ ->
     true
  | TCast (operand, _) | TImplicitCast (operand, _) | TNegation operand | TDeref (operand, _)
  | TSlotAccessor (operand, _, _) | TPointerSlotAccessor (operand, _, _) ->
     depends operand
  | TComparison (_, left, right) | TConjunction (left, right) | TDisjunction (left, right) ->
     depends left || depends right
  | TIfExpression (condition, if_true, if_false) ->
     depends condition || depends if_true || depends if_false
  | TRecordConstructor (_, values) | TUnionConstructor { args = values; _ } ->
     List.exists (fun (_, value) -> depends value) values
