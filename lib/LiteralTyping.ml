(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Common
open Type
open Stages.Tast
open Error
module Ast = Stages.Ast
module AstDB = Stages.AstDB

(* Folding *)

let rec fold_integer_arithmetic (place: place) (e: Ast.aexpr): Ast.aexpr =
  match try_folding' place e with
  | Some bi ->
     Ast.IntConstant (NumberLiteral.of_value bi)
  | None ->
     e

and try_folding' (place: place) (e: Ast.aexpr): Z.t option =
  match e with
  | Ast.IntConstant i ->
     Some i.value
  | Ast.ArithmeticExpression (op, lhs, rhs) -> begin
      match (try_folding' place lhs) with
      | Some lhs -> begin
          match (try_folding' place rhs) with
          | Some rhs -> begin
              Some (apply_op place op lhs rhs)
            end
          | None -> None
        end
      | None -> None
    end
  | Ast.NilConstant | Ast.BoolConstant _ | Ast.FloatConstant _ | Ast.StringConstant _
  | Ast.Variable _ | Ast.FunctionCall _ | Ast.ArithmeticNegation _ | Ast.Comparison _
  | Ast.Conjunction _ | Ast.Disjunction _ | Ast.Negation _ | Ast.IfExpression _
  | Ast.Path _ | Ast.RefPath _ | Ast.Embed _ | Ast.Deref _ | Ast.Typecast _
  | Ast.SizeOf _ | Ast.BorrowExpr _ | Ast.Reborrow _ ->
     (* A negation of a literal is a literal already (negation_of_operand). *)
     None

and apply_op (place: place) (op: arithmetic_operator) (lhs: Z.t) (rhs: Z.t): Z.t =
  match op with
  | Add -> Z.add lhs rhs
  | Subtract -> Z.sub lhs rhs
  | Multiply -> Z.mul lhs rhs
  | Divide when Z.equal rhs Z.zero -> type_error place DivisorIsZero
  | Divide -> Z.div lhs rhs

let negation_of_operand (operand: Ast.aexpr): Ast.aexpr =
  match operand with
  | Ast.IntConstant i ->
     Ast.IntConstant (NumberLiteral.of_value (Z.neg i.value))
  | Ast.FloatConstant f ->
     Ast.FloatConstant (NumberLiteral.negated_float f)
  | Ast.NilConstant | Ast.BoolConstant _ | Ast.StringConstant _ | Ast.Variable _
  | Ast.FunctionCall _ | Ast.ArithmeticExpression _ | Ast.ArithmeticNegation _
  | Ast.Comparison _ | Ast.Conjunction _ | Ast.Disjunction _ | Ast.Negation _
  | Ast.IfExpression _ | Ast.Path _ | Ast.RefPath _ | Ast.Embed _ | Ast.Deref _
  | Ast.Typecast _ | Ast.SizeOf _ | Ast.BorrowExpr _ | Ast.Reborrow _ ->
     Ast.ArithmeticNegation operand

(* Operands *)

let is_numeric_literal (e: AstDB.aexpr): bool =
  match e with
  | AstDB.IntConstant _ | AstDB.FloatConstant _ ->
     true
  | AstDB.NilConstant | AstDB.BoolConstant _ | AstDB.StringConstant _ | AstDB.Variable _
  | AstDB.Temporary _ | AstDB.FunctionCall _ | AstDB.ArithmeticExpression _
  | AstDB.ArithmeticNegation _
  | AstDB.Comparison _ | AstDB.Conjunction _ | AstDB.Disjunction _ | AstDB.Negation _
  | AstDB.IfExpression _ | AstDB.Embed _ | AstDB.Deref _ | AstDB.Typecast _
  | AstDB.SizeOf _ | AstDB.SlotAccessor _ | AstDB.PointerSlotAccessor _
  | AstDB.ArrayIndex _ ->
     false

(* A Literal Where a Type Is Expected *)

(* Check the given value fits in the given type. *)
let fits (signed : signedness) (width : integer_width) (n : Z.t) : bool =
  let min_bound, max_bound = match signed, width with
    | Unsigned, Width8        -> (Z.zero, Z.of_string "255")
    | Unsigned, Width16       -> (Z.zero, Z.of_string "65535")
    | Unsigned, Width32       -> (Z.zero, Z.of_string "4294967295")
    | Unsigned, Width64       -> (Z.zero, Z.of_string "18446744073709551615")
    (* A byte size and an index are both a size_t. TODO: Assuming 64-bit size_t *)
    | Unsigned, (WidthByteSize | WidthIndex) -> (Z.zero, Z.of_string "18446744073709551615")
    | Signed, Width8          -> (Z.of_string "-128", Z.of_string "127")
    | Signed, Width16         -> (Z.of_string "-32768", Z.of_string "32767")
    | Signed, Width32         -> (Z.of_string "-2147483648", Z.of_string "2147483647")
    | Signed, Width64         -> (Z.of_string "-9223372036854775808", Z.of_string "9223372036854775807")
    | Signed, (WidthByteSize | WidthIndex) -> (Z.of_string "-9223372036854775808", Z.of_string "9223372036854775807") (* TODO: Assuming 64-bit ssize_t *)
  in
  Z.compare min_bound n <= 0 && Z.compare n max_bound <= 0

let check_int_literal_fits (place: place) (signedness: signedness) (width: integer_width) (literal: NumberLiteral.int_literal): unit =
   (* Check that it fits *)
   if fits signedness width literal.value then
      ()
   else
      type_error place (IntegerDoesNotFit { literal; ty = Integer (signedness, width) })

let literal_matches (place: place) (ty: ty) (expr: texpr): bool =
  match ty with
  | Integer (signedness, width) ->
     (match expr with
      | TIntConstant value | TImplicitCast (TIntConstant value, _) ->
         check_int_literal_fits place signedness width value;
         true
      | TNilConstant | TBoolConstant _ | TFloatConstant _ | TStringConstant _
      | TConstVar _ | TParamVar _ | TLocalVar _ | TFunVar _ | TTemporary _ | TFuncall _
      | TMethodCall _ | TVarMethodCall _ | TFptrCall _ | TCast _ | TImplicitCast _
      | TComparison _ | TConjunction _ | TDisjunction _ | TNegation _ | TArithmeticNegation _
      | TIfExpression _
      | TRecordConstructor _ | TUnionConstructor _ | TSlotAccessor _
      | TPointerSlotAccessor _ | TArrayIndex _ | TSpanIndex _ | TEmbed _ | TDeref _
      | TSizeOf _ ->
         false)
  | SingleFloat | DoubleFloat ->
     (match expr with
      | TFloatConstant _ | TImplicitCast (TFloatConstant _, _) ->
         true
      | TNilConstant | TBoolConstant _ | TIntConstant _ | TStringConstant _
      | TConstVar _ | TParamVar _ | TLocalVar _ | TFunVar _ | TTemporary _ | TFuncall _
      | TMethodCall _ | TVarMethodCall _ | TFptrCall _ | TCast _ | TImplicitCast _
      | TComparison _ | TConjunction _ | TDisjunction _ | TNegation _ | TArithmeticNegation _
      | TIfExpression _
      | TRecordConstructor _ | TUnionConstructor _ | TSlotAccessor _
      | TPointerSlotAccessor _ | TArrayIndex _ | TSpanIndex _ | TEmbed _ | TDeref _
      | TSizeOf _ ->
         false)
  | Unit | Boolean | NamedType _ | RegionTy _ | ReadRef _ | WriteRef _ | Span _
  | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _ ->
     false

let annotation_types_literal (place: place) (ty: ty) (expr: texpr): bool =
  match ty with
  | Integer (signedness, width) ->
     (match expr with
      | TIntConstant value ->
         check_int_literal_fits place signedness width value;
         true
      | TNilConstant | TBoolConstant _ | TFloatConstant _ | TStringConstant _
      | TConstVar _ | TParamVar _ | TLocalVar _ | TFunVar _ | TTemporary _ | TFuncall _
      | TMethodCall _ | TVarMethodCall _ | TFptrCall _ | TCast _ | TImplicitCast _
      | TComparison _ | TConjunction _ | TDisjunction _ | TNegation _ | TArithmeticNegation _
      | TIfExpression _
      | TRecordConstructor _ | TUnionConstructor _ | TSlotAccessor _
      | TPointerSlotAccessor _ | TArrayIndex _ | TSpanIndex _ | TEmbed _ | TDeref _
      | TSizeOf _ ->
         false)
  | SingleFloat | DoubleFloat ->
     (match expr with
      | TFloatConstant _ ->
         true
      | TNilConstant | TBoolConstant _ | TIntConstant _ | TStringConstant _
      | TConstVar _ | TParamVar _ | TLocalVar _ | TFunVar _ | TTemporary _ | TFuncall _
      | TMethodCall _ | TVarMethodCall _ | TFptrCall _ | TCast _ | TImplicitCast _
      | TComparison _ | TConjunction _ | TDisjunction _ | TNegation _ | TArithmeticNegation _
      | TIfExpression _
      | TRecordConstructor _ | TUnionConstructor _ | TSlotAccessor _
      | TPointerSlotAccessor _ | TArrayIndex _ | TSpanIndex _ | TEmbed _ | TDeref _
      | TSizeOf _ ->
         false)
  | Unit | Boolean | NamedType _ | RegionTy _ | ReadRef _ | WriteRef _ | Span _
  | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _ ->
     false

(* A Literal Where No Type Reaches *)

let int_literal_default_type: ty = Integer (Signed, Width32)

let check_int_literal_fits_default_type (place: place) (expr: texpr): unit =
  (* The literal is as if annotated with its default type. *)
  let _ = annotation_types_literal place int_literal_default_type expr in
  ()
