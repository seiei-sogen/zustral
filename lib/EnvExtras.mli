(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Useful functions not part of the core environment API. *)
open Id
open Identifier
open Env
open EnvTypes

val get_decl_by_name_or_die : Error.place -> env -> sident -> decl

(** The name of a typeclass, with the name of its module. *)
val get_decl_sident_or_die : Error.place -> env -> typeclass_id -> sident

