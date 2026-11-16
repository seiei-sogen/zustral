(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** The linearity checker. *)
open Stages.Tast

(** The consumption state of a variable. *)
type var_state =
  | Unconsumed
  | BorrowedRead
  | BorrowedWrite
  | Consumed

(** The state table maps the variables in scope to their type and their
    consumption state. *)
type state_tbl

(** Represents the number of times a given variable appears in an expression in different ways. *)
type appearances

(** A module whose functions and methods all passed the linearity check. Only
    {!check_module_linearity} makes one, so a stage that takes this type
    cannot be given a module that skipped the check. *)
type checked_module

(** Check the linearity of all functions and methods in a module. *)
val check_module_linearity : typed_module -> checked_module Outcome.t

(** The module that passed the check. *)
val as_typed_module : checked_module -> typed_module
