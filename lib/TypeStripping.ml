(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Type
open Id
open Identifier
open Region
open Error

type stripped_ty =
  | SUnit
  | SBoolean
  | SInteger of signedness * integer_width
  | SSingleFloat
  | SDoubleFloat
  | SNamedType of qident * stripped_ty list
  | SRegionTy of region
  | SReadRef of stripped_ty * stripped_ty
  | SWriteRef of stripped_ty * stripped_ty
  | SSpan of stripped_ty * stripped_ty
  | SSpanMut of stripped_ty * stripped_ty
  | SAddress of stripped_ty
  | SPointer of stripped_ty
  | SFnPtr of stripped_ty list * stripped_ty
  | SMonoTy of mono_id
  | SRegionTyVar of identifier * qident

(* The arguments are stripped from left to right: stripping a type variable
   that was not replaced raises, and the first one decides the message. *)
let rec strip_type (ty: ty): stripped_ty =
  match ty with
  | Unit ->
     SUnit
  | Boolean ->
     SBoolean
  | Integer (s, w) ->
     SInteger (s, w)
  | SingleFloat ->
     SSingleFloat
  | DoubleFloat ->
     SDoubleFloat
  | NamedType (n, args, _) ->
     SNamedType (n, List.map strip_type args)
  | RegionTy r ->
     SRegionTy r
  | ReadRef (ty, r) ->
     let ty = strip_type ty in
     let r = strip_type (ty_of_ref_region r) in
     SReadRef (ty, r)
  | WriteRef (ty, r) ->
     let ty = strip_type ty in
     let r = strip_type (ty_of_ref_region r) in
     SWriteRef (ty, r)
  | Span (ty, r) ->
     let ty = strip_type ty in
     let r = strip_type (ty_of_ref_region r) in
     SSpan (ty, r)
  | SpanMut (ty, r) ->
     let ty = strip_type ty in
     let r = strip_type (ty_of_ref_region r) in
     SSpanMut (ty, r)
  | TyVar (TypeVariable (name, u, source, _)) ->
     (match u with
      | RegionUniverse ->
         SRegionTyVar (name, source)
      | FreeUniverse | LinearUniverse | TypeUniverse ->
         (* Why? Because when instantiating a monomorph, we do search and replace
            of type variables with their substitutions. So if there are variables
            left over by stripping time, that's an error. Anyways, the
            search-and-replace step should *also* have signalled an error if a
            type variable has no replacement. *)
         generic_error NoSpan (TypeVariableNotReplaced name))
  | Address ty ->
     SAddress (strip_type ty)
  | Pointer ty ->
     SPointer (strip_type ty)
  | FnPtr (args, rt) ->
     let rt = strip_type rt in
     SFnPtr (List.map strip_type args, rt)
  | MonoTy id ->
     SMonoTy id
