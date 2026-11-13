(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Type
open Error

type direction =
  | Import
  | Export

type 'leaf foreign_ty =
  | ForeignBoolean
  | ForeignInteger of signedness * integer_width
  | ForeignSingleFloat
  | ForeignDoubleFloat
  | ForeignFnPtr
  | ForeignAddress of 'leaf foreign_ty
  | ForeignPointer of 'leaf
  | ForeignSpan of 'leaf

type 'leaf foreign_signature = {
    params: (identifier * 'leaf foreign_ty) list;
    rt: 'leaf foreign_ty;
  }

(* The two rule sets differ, and are kept as they were when imports were checked
   in code generation and exports when the wrappers were made. *)
let rec check_ty (place: place) (direction: direction) (ty: ty): ty foreign_ty =
  match direction with
  | Import ->
     let not_allowed () = generic_error place ForeignFunctionTypeNotAllowed in
     (match ty with
      | Boolean ->
         ForeignBoolean
      | Integer (s, w) ->
         ForeignInteger (s, w)
      | SingleFloat ->
         ForeignSingleFloat
      | DoubleFloat ->
         ForeignDoubleFloat
      | Span (t, _) ->
         ForeignSpan t
      | SpanMut (t, _) ->
         ForeignSpan t
      | Address t ->
         ForeignPointer t
      | Pointer t ->
         ForeignPointer t
      | FnPtr _ ->
         ForeignFnPtr
      (* Only a generic signature has type variables, and generic imports are
         not checked here (EnvTypes.ForeignGeneric). In code generation, a
         region variable was rejected. *)
      | Unit | NamedType _ | MonoTy _ | RegionTy _ | ReadRef _ | WriteRef _ | TyVar _ ->
         not_allowed ())
  | Export ->
     let not_allowed () = type_error place (ExportDisallowedType ty) in
     (match ty with
      | Boolean ->
         ForeignBoolean
      | Integer (s, w) ->
         ForeignInteger (s, w)
      | SingleFloat ->
         ForeignSingleFloat
      | DoubleFloat ->
         ForeignDoubleFloat
      | Address t ->
         ForeignAddress (check_ty place Export t)
      | FnPtr _ ->
         ForeignFnPtr
      | Unit | NamedType _ | MonoTy _ | RegionTy _ | ReadRef _ | WriteRef _
      | Span _ | SpanMut _ | Pointer _ | TyVar _ ->
         not_allowed ())

(* The return type is checked before the parameters, as the wrappers of exported
   functions did: it decides which error a signature with two reports. *)
let check_signature (place: place) (direction: direction) (params: value_parameter list) (rt: ty): ty foreign_signature =
  let rt = check_ty place direction rt in
  let params = List.map (fun (ValueParameter (name, ty)) -> (name, check_ty place direction ty)) params in
  { params; rt }
