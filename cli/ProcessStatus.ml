(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Unix

external system_signal_number : int -> int = "zustral_system_signal_number" [@@noalloc]

let exit_code (status: process_status): int =
  match status with
  | WEXITED i -> i
  | WSIGNALED signal | WSTOPPED signal -> 128 + system_signal_number signal
