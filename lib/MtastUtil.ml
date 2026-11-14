(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open MonoType
open Stages.Mtast
open Error

let mono_reference_type (elem_ty: mono_ty) (region: mono_ty): Common.borrowing_mode -> mono_ty = function
  | ReadBorrow -> MonoReadRef (elem_ty, region)
  | WriteBorrow -> MonoWriteRef (elem_ty, region)

let rec get_type (e: mexpr): mono_ty =
  match e with
  | MNilConstant ->
     MonoUnit
  | MBoolConstant _ ->
     MonoBoolean
  | MIntConstant _ ->
     MonoInteger (Signed, Width32)
  | MFloatConstant _ ->
     MonoDoubleFloat
  | MStringConstant _ ->
     generic_error NoSpan TypeOfStringConstant
  | MConstVar (_, ty) ->
     ty
  | MParamVar (_, ty) ->
     ty
  | MLocalVar (_, ty) ->
     ty
  | MTemporary (_, ty) ->
     ty
  | MGenericFunVar (_, ty) ->
     ty
  | MConcreteFunVar (_, ty) ->
     ty
  | MConcreteFuncall (_, _, _, ty) ->
     ty
  | MGenericFuncall (_, _, ty) ->
     ty
  | MConcreteMethodCall (_, _, _, ty) ->
     ty
  | MGenericMethodCall (_, _, _, ty) ->
     ty
  | MFptrCall (_, _, rt) ->
     rt
  | MCast (_, ty) ->
     ty
  | MComparison _ ->
     MonoBoolean
  | MConjunction _ ->
     MonoBoolean
  | MDisjunction _ ->
     MonoBoolean
  | MNegation _ ->
     MonoBoolean
  | MIfExpression (_, t, _) ->
     get_type t
  | MRecordConstructor (ty, _) ->
     ty
  | MUnionConstructor (id, _, _) ->
     MonoNamedType (Id.mono_id_of_union id)
  | MEmbed (ty, _, _) ->
     ty
  | MDeref (_, ty) ->
     ty
  | MSizeOf _ ->
     MonoInteger (Unsigned, Width64)
  | MSlotAccessor (_, _, ty) -> ty
  | MPointerSlotAccessor (_, _, ty) -> ty
  | MArrayIndex (_, _, elem_ty, region, mode) -> mono_reference_type elem_ty region mode
  | MSpanIndex (_, _, elem_ty, region, mode) -> mono_reference_type elem_ty region mode
