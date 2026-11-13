(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Id
open Identifier
open Env
open EnvTypes
open Error

let get_decl_by_name_or_die (place: place) (env: env) (name: sident): decl =
  match get_decl_by_name place env name with
  | Some decl -> decl
  | None ->
     (* A program reaches this: an instance declared before the type it is for,
        in the same module. The orphan rule looks the type up before the type
        is registered. *)
     internal_err place ("No declaration with name `"
                   ^ ident_string (sident_name name)
                   ^ "`.")

let get_decl_sident_or_die (place: place) (env: env) (id: typeclass_id): sident =
  let { mod_id; name; _ }: typeclass_decl = get_typeclass place env id in
  make_sident (module_name_from_id place env mod_id) name
