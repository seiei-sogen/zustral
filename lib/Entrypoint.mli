(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Entrypoint logic. *)
open Env
open Identifier

(** Given the env, and the name of the entrypoint function, generate the
    entrypoint code. Also returns the declarations of the monomorphs of the
    types of its signature that the environment did not have yet, to be
    generated with the program. *)
val entrypoint_code : env -> qident -> (env * Stages.Mtast.mdecl list * CRepr.c_source) Outcome.t
