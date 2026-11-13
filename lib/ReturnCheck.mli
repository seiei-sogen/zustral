(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** This module implements a check over the TAST that a function ends with
    either a return statement or a call to `Abort`. *)
open Stages.Combined

(** A module whose functions and methods all end in a return statement or a
    call to `Abort`. Only {!check_ends_in_return} makes one, so a stage that
    takes this type cannot be given a module that skipped the check. *)
type returning_module

val check_ends_in_return : combined_module -> returning_module Outcome.t

(** The module that passed the check. *)
val as_combined_module : returning_module -> combined_module
