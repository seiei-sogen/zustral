(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Type
open Id
open Env
open EnvTypes
open Error

(* The typeclass that a constraint of the type variable names, or the type error
   that says what the name is instead. Extraction looks up every constraint
   where it is declared (#128), so a call that needs an instance for a
   constraint finds a typeclass. *)
let typeclass_named_by_constraint (place: place) (env: env) (tyvar: type_var) (typeclass: sident): (typeclass_id, error) result =
  match get_decl_by_name place env typeclass with
  | Some (TypeClass { id; _ }) ->
     Ok id
  | Some (Constant _ | Record _ | Union _ | UnionCase _ | Function _ | TypeClassMethod _) ->
     Error (Type (ConstraintNotTypeclass { tyvar; typeclass }))
  | None ->
     Error (Type (ConstraintUnknownTypeclass { tyvar; typeclass }))
