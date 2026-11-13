(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(* A catch-all arm over a variant type is an error in this file: a new
   constructor must stop the build at every match. *)
[@@@warning "@4"]

open Type
open TypeVarSet
open Error

let type_universe (place: place) = function
    Unit -> FreeUniverse
  | Boolean -> FreeUniverse
  | Integer _ -> FreeUniverse
  | SingleFloat -> FreeUniverse
  | DoubleFloat -> FreeUniverse
  | NamedType (_, _, u) -> u
  | RegionTy _ -> RegionUniverse
  | ReadRef _ -> FreeUniverse
  | WriteRef _ -> LinearUniverse
  | Span _ -> FreeUniverse
  | SpanMut _ -> LinearUniverse
  | TyVar (TypeVariable (_, u, _, _)) -> u
  | Address _ -> FreeUniverse
  | Pointer _ -> FreeUniverse
  | FnPtr _ -> FreeUniverse
  | MonoTy _ -> internal_err place "You shouldn't be asking for the type_universe of a MonoTy"

let is_numeric (place: place) = function
  | Unit -> false
  | Boolean -> false
  | Integer _ -> true
  | SingleFloat -> true
  | DoubleFloat -> true
  | NamedType _ -> false
  | RegionTy _ -> false
  | ReadRef _ -> false
  | WriteRef _ -> false
  | Span _ -> false
  | SpanMut _ -> false
  | TyVar _ -> false
  | Address _ -> false
  | Pointer _ -> false
  | FnPtr _ -> false
  | MonoTy _ -> generic_error place IsNumericOnMonoTy

let rec type_variables = function
  | Unit ->
     TypeVarSet.empty
  | Boolean ->
     TypeVarSet.empty
  | Integer _ ->
     TypeVarSet.empty
  | SingleFloat ->
     TypeVarSet.empty
  | DoubleFloat ->
     TypeVarSet.empty
  | NamedType (_, a, _) ->
     List.fold_left TypeVarSet.union TypeVarSet.empty (List.map type_variables a)
  | RegionTy _ ->
     TypeVarSet.empty
  | ReadRef (ty, r) ->
     TypeVarSet.union (type_variables ty) (type_variables (ty_of_ref_region r))
  | WriteRef (ty, r) ->
     TypeVarSet.union (type_variables ty) (type_variables (ty_of_ref_region r))
  | Span (ty, r) ->
     TypeVarSet.union (type_variables ty) (type_variables (ty_of_ref_region r))
  | SpanMut (ty, r) ->
     TypeVarSet.union (type_variables ty) (type_variables (ty_of_ref_region r))
  | TyVar v ->
     TypeVarSet.singleton v
  | Address ty ->
     type_variables ty
  | Pointer ty ->
     type_variables ty
  | FnPtr (args, rt) ->
     let args' = List.fold_left TypeVarSet.union TypeVarSet.empty (List.map type_variables args)
     and rt' = type_variables rt
     in
     TypeVarSet.union args' rt'
  | MonoTy _ ->
    TypeVarSet.empty

(** Given a list of types, return whether any of them are in the Linear
    universe. *)
and any_arg_is_linear (place: place) (args: ty list) =
  let is_linear = function
    | LinearUniverse -> true
    | FreeUniverse | TypeUniverse | RegionUniverse -> false
  in
  List.exists is_linear (List.map (type_universe place) args)

(** Given a list of types, return whether any of them are in the Type
    universe. *)
and any_arg_is_type (place: place) (args: ty list) =
  let is_type = function
    | TypeUniverse -> true
    | FreeUniverse | LinearUniverse | RegionUniverse -> false
  in
  List.exists is_type (List.map (type_universe place) args)
