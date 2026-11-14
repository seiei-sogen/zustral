(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** This module creates wrappers around exported functions. *)
open Id
open MonoType
open CRepr
open ForeignType

(** Make the wrappers of the exported functions. Each is given as
    {!Monomorphize.monomorphize_wrappers} returns it: its export name, the ID
    of the monomorph the wrapper calls and its checked signature. *)
val all_wrappers : (string * mono_id * mono_ty foreign_signature) list -> c_decl list Outcome.t
