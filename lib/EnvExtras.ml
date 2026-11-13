(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Id
open Identifier
open Env
open EnvTypes
open EnvUtils
open Error

let get_decl_by_name_or_die (env: env) (name: sident): decl =
  match get_decl_by_name env name with
  | Some decl -> decl
  | None ->
     (* A program reaches this: an instance declared before the type it is for,
        in the same module. The orphan rule looks the type up before the type
        is registered. *)
     internal_err ("No declaration with name `"
                   ^ ident_string (sident_name name)
                   ^ "`.")

let get_decl_sident_or_die (env: env) (id: decl_id): sident =
  let decl: decl = get_decl_by_id env id in
  match (decl_name decl) with
  | Some name ->
     make_sident (module_name_from_id env (decl_mod_id decl)) name
  | None ->
     generic_error DeclHasNoName
