(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** This module creates wrappers around exported functions. *)
open Id
open Env
open CRepr

(** Make the wrappers of the exported functions. Each is given as
    {!Monomorphize.monomorphize_wrappers} returns it: its ID, its export name
    and the ID of the monomorph the wrapper calls. *)
val all_wrappers : env -> (function_id * string * mono_id) list -> c_decl list Outcome.t
