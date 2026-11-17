(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Type
open Stages.Tast
open Identifier
open Error

let arglist_size = function
  | (TPositionalArglist l) -> List.length l
  | (TNamedArglist l) -> List.length l

let rec arglist_to_positional (place: place) (args, names): texpr list =
  let expected = List.length names
  and actual = arglist_size args
  in
  if expected = actual then
    match args with
    | (TPositionalArglist l) -> l
    | (TNamedArglist l) ->
       check_argument_names place (List.map fst l) names;
       reorder_arglist l names
  else
    generic_error place (WrongNumberOfArguments { expected; actual })

(* As many names as parameters, each the name of a parameter and none given
   twice: the arguments are the parameters in some order. *)
and check_argument_names (place: place) (given: identifier list) (parameters: identifier list): unit =
  let check (earlier: identifier list) (name: identifier): identifier list =
    if not (List.mem name parameters) then
      type_error place (CallArgumentNameUnknown { name; parameters })
    else if List.mem name earlier then
      type_error place (CallArgumentNameRepeated name)
    else
      name :: earlier
  in
  let _ = List.fold_left check [] given in
  ()

and reorder_arglist l names =
  List.map (fun n -> find_arg_with_name l n) names

and find_arg_with_name l name =
  let (_, value) = List.find (fun (n, _) -> name = n) l in
  value

let reference_type (elem_ty: ty) (region: ref_region): Common.borrowing_mode -> ty = function
  | ReadBorrow -> ReadRef (elem_ty, region)
  | WriteBorrow -> WriteRef (elem_ty, region)

let rec get_type (place: place) = function
  | TNilConstant ->
     Unit
  | TBoolConstant _ ->
     Boolean
  | TIntConstant _ ->
     LiteralTyping.int_literal_default_type
  | TFloatConstant _ ->
     DoubleFloat
  | TStringConstant _ ->
     string_type
  | TConstVar (_, ty) ->
     ty
  | TParamVar (_, ty) ->
     ty
  | TLocalVar (_, ty) ->
     ty
  | TFunVar (_, ty, _) ->
     ty
  | TTemporary (_, ty) ->
     ty
  | TFuncall (_, _, _, ty, _) ->
     ty
  | TMethodCall (_, _, _, _, ty, _) ->
     ty
  | TVarMethodCall { rt; _ } ->
     rt
  | TFptrCall (_, _, _, rt) ->
     rt
  | TCast (_, ty) | TImplicitCast (_, ty) ->
     ty
  | TComparison _ ->
     Boolean
  | TConjunction _ ->
     Boolean
  | TDisjunction _ ->
     Boolean
  | TNegation _ ->
     Boolean
  | TIfExpression (_, t, _) ->
     get_type place t
  | TRecordConstructor (ty, _) ->
     ty
  | TUnionConstructor { union_id; type_name; tyargs; universe; _ } ->
     NamedType (type_name, Id.UnionTypeId union_id, tyargs, universe)
  | TEmbed (ty, _, _) ->
     ty
  | TDeref (_, ty) ->
     ty
  | TSizeOf _ ->
     Integer (Unsigned, WidthByteSize)
  | TSlotAccessor (_, _, ty) -> ty
  | TPointerSlotAccessor (_, _, ty) -> ty
  | TArrayIndex (_, _, elem_ty, region, mode) -> reference_type elem_ty region mode
  | TSpanIndex (_, _, elem_ty, region, mode) -> reference_type elem_ty region mode

let rec is_constant_expression (expr: texpr): bool =
  match expr with
  | TNilConstant ->
     true
  | TBoolConstant _ ->
     true
  | TIntConstant _ ->
     true
  | TFloatConstant _ ->
     true
  | TStringConstant _ ->
     true
  | TConstVar _ ->
     true
  | TParamVar _ ->
     false
  | TLocalVar _ ->
     false
  | TTemporary _ ->
     false
  | TFunVar _ ->
     true
  | TFuncall _ ->
     false
  | TMethodCall _ ->
     false
  | TVarMethodCall _ ->
     false
  | TFptrCall _ ->
     false
  | TCast _ | TImplicitCast _ ->
     true
  | TComparison (_, lhs, rhs) ->
     (is_constant_expression lhs) && (is_constant_expression rhs)
  | TConjunction (lhs, rhs) ->
     (is_constant_expression lhs) && (is_constant_expression rhs)
  | TDisjunction (lhs, rhs) ->
     (is_constant_expression lhs) && (is_constant_expression rhs)
  | TNegation e ->
     is_constant_expression e
  | TIfExpression (c, t, f) ->
     (is_constant_expression c) && (is_constant_expression t) && (is_constant_expression f)
  | TRecordConstructor (_, values) ->
     List.for_all (fun (_, v) -> is_constant_expression v) values
  | TUnionConstructor { args = values; _ } ->
     List.for_all (fun (_, v) -> is_constant_expression v) values
  | TEmbed _ ->
     true
  | TDeref _ ->
     false
  | TSizeOf _ ->
     true
  | TSlotAccessor _ ->
     true
  | TPointerSlotAccessor _ ->
     true
  | TArrayIndex (_, idx, _, _, _) ->
     is_constant_expression idx
  | TSpanIndex (_, idx, _, _, _) ->
     is_constant_expression idx
