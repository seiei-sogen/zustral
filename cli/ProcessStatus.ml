(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Unix

let exit_code (status: process_status): int =
  match status with
  | WEXITED i -> i
  | WSIGNALED i -> i
  | WSTOPPED i -> i
