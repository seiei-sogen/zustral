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

let get_decl_sident_or_die (place: place) (env: env) (id: typeclass_id): sident =
  let { mod_id; name; _ }: typeclass_decl = get_typeclass place env id in
  make_sident (module_name_from_id place env mod_id) name
