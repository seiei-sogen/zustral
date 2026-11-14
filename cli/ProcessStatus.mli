(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The status of a finished process, as a code. *)

(** The exit code of a process that exited, or the number of the signal that
    killed or stopped it (OCaml's number, which is negative for the signals
    it knows). *)
val exit_code : Unix.process_status -> int
