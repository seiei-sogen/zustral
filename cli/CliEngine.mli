(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Executes parsed CLI commands. *)
open Zustral_core
open CliParser
open Error

(** Run a command. Raises [Zustral_error] for errors in the command itself;
    reports compile errors and exits. *)
val exec : cmd -> unit

(** Write the error to stderr and to error.html, then exit with a failure
    status. *)
val report_and_exit : error_reporting_mode -> zustral_error -> 'a
