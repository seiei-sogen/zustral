(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Id
open Identifier
open IdentifierSet
open Type
open TypeParameters
open TypeParser
open TypeSystem
open Env
open EnvTypes
open EnvExtras
open EnvUtils
open Error
open Error.Placed

let check_instance_argument_has_right_universe (place: place) (universe: universe) (arg: ty): unit =
  if universe_compatible universe (type_universe arg) then
    ()
  else
    type_error place (InstanceArgumentWrongUniverse {
        ty = arg;
        expected = universe;
        actual = type_universe arg;
      })

let check_instance_argument_has_right_shape (place: place) (typarams: typarams) (arg: ty): unit =
  let is_tyvar (ty: ty): type_var =
    match ty with
    | TyVar tv ->
       tv
    | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
    | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | Address _ | Pointer _ | FnPtr _
    | MonoTy _ ->
       declaration_error place (InstanceArgumentNotATyvar { ty; universe = type_universe ty })
  and all_distinct (tyvars: type_var list): unit =
    let names: identifier list = List.map (fun (TypeVariable (name, _, _, _)) -> name) tyvars in
    let set: IdentifierSet.t = IdentifierSet.of_list names in
    if List.length tyvars = IdentifierSet.cardinal set then
      ()
    else
      declaration_error place InstanceDuplicateTyvars
  (* Assert the number of type parameters *)
  and no_leftovers (n: int): unit =
    if (typarams_size typarams) = n then
      ()
    else
      declaration_error place InstanceLeftoverTypeParameters
  in
  match arg with
  | Unit -> ()
  | Boolean -> ()
  | Integer _ -> ()
  | SingleFloat -> ()
  | DoubleFloat -> ()
  | NamedType (_, args, _) ->
     let _ = all_distinct (List.map is_tyvar args) in
     let _ = no_leftovers (List.length args) in
     ()
  | RegionTy _ ->
     declaration_error place InstanceArgumentRegion
  | ReadRef (ty, r) ->
     let _ = all_distinct [is_tyvar ty; is_tyvar (ty_of_ref_region r)] in
     let _ = no_leftovers 2 in
     ()
  | WriteRef (ty, r) ->
     let _ = all_distinct [is_tyvar ty; is_tyvar (ty_of_ref_region r)] in
     let _ = no_leftovers 2 in
     ()
  | Span (ty, r) ->
     let _ = all_distinct [is_tyvar ty; is_tyvar (ty_of_ref_region r)] in
     let _ = no_leftovers 2 in
     ()
  | SpanMut (ty, r) ->
     let _ = all_distinct [is_tyvar ty; is_tyvar (ty_of_ref_region r)] in
     let _ = no_leftovers 2 in
     ()
  | TyVar _ ->
     declaration_error place InstanceArgumentLoneTyvar
  | Address ty ->
     let _ = is_tyvar ty in
     let _ = no_leftovers 1 in
     ()
  | Pointer ty ->
     let _ = is_tyvar ty in
     let _ = no_leftovers 1 in
     ()
  | FnPtr (args, ty) ->
     let args' = ty :: args in
     let _ = all_distinct (List.map is_tyvar args') in
     let _ = no_leftovers (List.length args') in
     ()
  | MonoTy _ ->
     generic_error place InstanceArgumentShapeNotAllowed

let overlapping_instances (place: place) (a: ty) (b: ty): bool =
  match a with
  | Unit ->
     (match b with
      | Unit -> true
      | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
      | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ -> false)
  | Boolean ->
     (match b with
      | Boolean -> true
      | Unit | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
      | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ -> false)
  | Integer (s, w) ->
     (match b with
      | Integer (s', w') ->
         (equal_signedness s s')
         && (equal_integer_width w w')
      | Unit | Boolean | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
      | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ ->
         false)
  | SingleFloat ->
     (match b with
      | SingleFloat -> true
      | Unit | Boolean | Integer _ | DoubleFloat | NamedType _ | RegionTy _ | ReadRef _
      | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
      | MonoTy _ -> false)
  | DoubleFloat ->
     (match b with
      | DoubleFloat -> true
      | Unit | Boolean | Integer _ | SingleFloat | NamedType _ | RegionTy _ | ReadRef _
      | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
      | MonoTy _ -> false)
  | NamedType (name, _, _) ->
     (match b with
      | NamedType (name', _, _) ->
         equal_qident name name'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _ | ReadRef _
      | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
      | MonoTy _ ->
         false)
  | RegionTy _ ->
     generic_error place InstanceOverlapTypeNotAllowed
  | ReadRef _ ->
     (match b with
      | ReadRef _ -> true
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ -> false)
  | WriteRef _ ->
     (match b with
      | WriteRef _ -> true
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ -> false)
  | Span _ ->
     (match b with
      | Span _ -> true
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | SpanMut _ | TyVar _ | Address _
      | Pointer _ | FnPtr _ | MonoTy _ -> false)
  | SpanMut _ ->
     (match b with
      | SpanMut _ -> true
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ -> false)
  | TyVar _ ->
     generic_error place InstanceOverlapTypeNotAllowed
  | Address _ ->
     (match b with
      | Address _ -> true
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Pointer _
      | FnPtr _ | MonoTy _ -> false)
  | Pointer _ ->
     (match b with
      | Pointer _ -> true
      (* Issue #122: a Pointer instance overlaps an instance for any other
         type. These should be false; they stay true until the issue is fixed
         (pinned by 037-typeclass-and-environment-paths/013). *)
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _
      | FnPtr _ | MonoTy _ -> true)
  | FnPtr _ ->
     (match b with
      | FnPtr _ -> true
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _
      | Pointer _ | MonoTy _ -> false)
  | MonoTy _ ->
     generic_error place InstanceOverlapTypeNotAllowed

let check_instance_locally_unique (place: place) (instances: instance_decl list) (argument: ty): unit =
  let pred (instance: instance_decl): bool =
    overlapping_instances place argument instance.argument
  in
  if List.exists pred instances then
    generic_error place OverlappingInstances
  else
    ()

let rec check_instance_orphan_rules (place: place) (env: env) (mod_id: mod_id) (typeclass_mod_id: mod_id) (ty: ty): unit =
  let tc_local: bool = equal_mod_id mod_id typeclass_mod_id
  and ty_local: bool = is_type_local env mod_id ty
  in
  if (not tc_local) && (not ty_local) then
    generic_error place BrokenOrphanRule
  else
    ()

and is_type_local (env: env) (mod_id: mod_id) (ty: ty): bool =
  match ty with
  | Unit ->
     true
  | Boolean ->
     true
  | Integer _ ->
     true
  | SingleFloat ->
     true
  | DoubleFloat ->
     true
  | NamedType (name, _, _) ->
     let type_mod_id: mod_id = decl_mod_id (get_decl_by_name_or_die env (qident_to_sident name)) in
     equal_mod_id mod_id type_mod_id
  | RegionTy _ ->
     (* Not applicable. *)
     false
  | ReadRef _ ->
     true
  | WriteRef _ ->
     true
  | Span _ ->
     true
  | SpanMut _ ->
     true
  | TyVar _ ->
     (* Not applicable. *)
     false
  | Address _ ->
     true
  | Pointer _ ->
     true
  | FnPtr _ ->
     true
  | MonoTy _ ->
     (* Not applicable. *)
     false

let check_disjoint_typarams (place: place) (name: identifier) (typarams: typarams): unit =
  match get_typaram typarams name with
  | Some _ ->
     declaration_error place (InstanceShadowsTypeclassParam name)
  | None ->
     (* No collision *)
     ()
