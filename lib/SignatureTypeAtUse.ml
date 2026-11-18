(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Type

let type_variable_bound_at_use (TypeVariable (name, universe, declaration, constraints, _): type_var): type_var =
  TypeVariable (name, universe, declaration, constraints, BoundAtUse)

let rec signature_type_at_use (ty: ty): ty =
  match ty with
  | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _ ->
     ty
  | NamedType (name, id, args, universe) ->
     NamedType (name, id, List.map signature_type_at_use args, universe)
  | ReadRef (ty, region) ->
     ReadRef (signature_type_at_use ty, signature_region_at_use region)
  | WriteRef (ty, region) ->
     WriteRef (signature_type_at_use ty, signature_region_at_use region)
  | Span (ty, region) ->
     Span (signature_type_at_use ty, signature_region_at_use region)
  | SpanMut (ty, region) ->
     SpanMut (signature_type_at_use ty, signature_region_at_use region)
  | TyVar tv ->
     TyVar (type_variable_bound_at_use tv)
  | Address ty ->
     Address (signature_type_at_use ty)
  | Pointer ty ->
     Pointer (signature_type_at_use ty)
  | FnPtr (args, rt) ->
     FnPtr (List.map signature_type_at_use args, signature_type_at_use rt)

and signature_region_at_use (region: ref_region): ref_region =
  match region with
  | RefRegion _ ->
     region
  | RefRegionVar tv ->
     RefRegionVar (type_variable_bound_at_use tv)
