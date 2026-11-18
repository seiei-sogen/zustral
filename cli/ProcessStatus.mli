(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The status of a finished process, as a code. *)

(** The exit code of a process that exited, or, as a shell reports it, 128
    and the system's number of the signal that killed or stopped it. *)
val exit_code : Unix.process_status -> int
