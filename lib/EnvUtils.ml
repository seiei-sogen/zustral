(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Common
open Id
open EnvTypes
open Type

let decl_mod_id (decl: decl): mod_id =
  match decl with
  | Constant { mod_id; _ } -> mod_id
  | Record { mod_id; _ } -> mod_id
  | Union { mod_id; _ } -> mod_id
  | UnionCase { mod_id; _ } -> mod_id
  | Function { mod_id; _ } -> mod_id
  | TypeClass { mod_id; _ } -> mod_id
  | TypeClassMethod { mod_id; _ } -> mod_id

let decl_name (decl: decl): identifier =
  match decl with
  | Constant { name; _ } -> name
  | Record { name; _ } -> name
  | Union { name; _ } -> name
  | UnionCase { name; _ } -> name
  | Function { name; _ } -> name
  | TypeClass { name; _ } -> name
  | TypeClassMethod { name; _ } -> name

let is_importable (decl: decl): bool =
  let importable_vis = function
    | VisPublic -> true
    | VisPrivate -> false
  and importable_type = function
    | TypeVisPublic -> true
    | TypeVisOpaque -> true
    | TypeVisPrivate -> false
  in
  match decl with
  | Constant { vis; _ } -> importable_vis vis
  | Record { vis; _ } -> importable_type vis
  | Union { vis; _ } -> importable_type vis
  | UnionCase { vis; _ } -> importable_vis vis
  | Function { vis; _ } -> importable_vis vis
  | TypeClass { vis; _ } -> importable_vis vis
  | TypeClassMethod { vis; _ } -> importable_vis vis

let union_case_to_typed_case (case: union_case_decl): typed_case =
  TypedCase (case.name, case.slots)
