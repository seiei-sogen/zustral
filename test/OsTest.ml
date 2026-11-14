(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Austral_cli.Os
open TestUtil

let test_run_command _ =
  let (CommandOutput { command; code; stdout; stderr }) = run_command "echo derp" in
  eq "echo derp" command;
  eq 0 code;
  eq "derp" stdout;
  eq "" stderr

(* `run_command` never sees a stopped process: it waits for the command to
   finish. *)
let test_exit_code _ =
  eq 3 (Austral_cli.ProcessStatus.exit_code (Unix.WEXITED 3));
  eq Sys.sigkill (Austral_cli.ProcessStatus.exit_code (Unix.WSIGNALED Sys.sigkill));
  eq Sys.sigstop (Austral_cli.ProcessStatus.exit_code (Unix.WSTOPPED Sys.sigstop))

let suite =
  "Operating system" >::: [
      "run_command" >:: test_run_command;
      "exit_code" >:: test_exit_code
    ]

let _ = run_test_tt_main suite
