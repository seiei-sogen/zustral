(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
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

type 'leaf foreign_return =
  | ForeignReturnsNothing
  | ForeignReturns of 'leaf foreign_ty

type 'leaf foreign_signature = {
    params: (identifier * 'leaf foreign_ty) list;
    rt: 'leaf foreign_return;
  }

(* The two rule sets differ, and are kept as they were when imports were checked
   in code generation and exports when the wrappers were made. `parameter` is
   the name of the parameter of type `ty`, or None for the return type. *)
let rec check_ty (place: place) (direction: direction) (function_name: identifier) (parameter: identifier option) (ty: ty): ty foreign_ty =
  match direction with
  | Import ->
     let not_allowed () = type_error place (ImportDisallowedType { function_name; parameter; ty }) in
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
      | Unit | NamedType _ | RegionTy _ | ReadRef _ | WriteRef _ | TyVar _ ->
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
         ForeignAddress (check_ty place Export function_name parameter t)
      | FnPtr _ ->
         ForeignFnPtr
      | Unit | NamedType _ | RegionTy _ | ReadRef _ | WriteRef _
      | Span _ | SpanMut _ | Pointer _ | TyVar _ ->
         not_allowed ())

(* The return type is checked before the parameters, as the wrappers of exported
   functions did: it decides which error a signature with two reports. *)
let check_signature (place: place) (direction: direction) (function_name: identifier) (params: value_parameter list) (rt: ty): ty foreign_signature =
  (* An imported function may return nothing, as a C function that returns
     `void` does (#54). *)
  let returns_nothing: bool =
    match direction with
    | Import -> equal_ty rt Unit
    | Export -> false
  in
  let rt = if returns_nothing then ForeignReturnsNothing else ForeignReturns (check_ty place direction function_name None rt) in
  let params = List.map (fun (ValueParameter (name, ty)) -> (name, check_ty place direction function_name (Some name) ty)) params in
  { params; rt }

let rec map_ty (f: 'state -> 'leaf -> 'mapped * 'state) (state: 'state) (ty: 'leaf foreign_ty): 'mapped foreign_ty * 'state =
  match ty with
  | ForeignBoolean ->
     (ForeignBoolean, state)
  | ForeignInteger (s, w) ->
     (ForeignInteger (s, w), state)
  | ForeignSingleFloat ->
     (ForeignSingleFloat, state)
  | ForeignDoubleFloat ->
     (ForeignDoubleFloat, state)
  | ForeignFnPtr ->
     (ForeignFnPtr, state)
  | ForeignAddress t ->
     let (t, state) = map_ty f state t in
     (ForeignAddress t, state)
  | ForeignPointer t ->
     let (t, state) = f state t in
     (ForeignPointer t, state)
  | ForeignSpan t ->
     let (t, state) = f state t in
     (ForeignSpan t, state)

let map_signature (f: 'state -> 'leaf -> 'mapped * 'state) (state: 'state) ({ params; rt }: 'leaf foreign_signature): 'mapped foreign_signature * 'state =
  let (rt, state) =
    match rt with
    | ForeignReturnsNothing ->
       (ForeignReturnsNothing, state)
    | ForeignReturns ty ->
       let (ty, state) = map_ty f state ty in
       (ForeignReturns ty, state)
  in
  let (state, params) =
    Util.map_with_context
      (fun (state, (name, ty)) ->
        let (ty, state) = map_ty f state ty in
        (state, (name, ty)))
      state
      params
  in
  ({ params; rt }, state)
