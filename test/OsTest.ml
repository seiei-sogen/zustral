(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Zustral_cli.Os
open TestUtil

let test_run_command _ =
  let (CommandOutput { command; code; stdout; stderr }) = run_command "echo derp" in
  eq "echo derp" command;
  eq 0 code;
  eq "derp" stdout;
  eq "" stderr

(* A signal gives 128 and the system's number of the signal, as a shell
   reports it (issue #144). SIGKILL is 9 and SIGTERM 15 on every system.
   `run_command` never sees a stopped process: it waits for the command to
   finish. *)
let test_exit_code _ =
  eq 3 (Zustral_cli.ProcessStatus.exit_code (Unix.WEXITED 3));
  eq 137 (Zustral_cli.ProcessStatus.exit_code (Unix.WSIGNALED Sys.sigkill));
  eq 143 (Zustral_cli.ProcessStatus.exit_code (Unix.WSTOPPED Sys.sigterm))

(* Issue #93: a file of a million lines is read without running out of
   stack. A command-line test would need a fixture of megabytes. *)
let test_read_file_of_a_million_lines _ =
  let line_count = 1_000_000 in
  let path = Filename.temp_file "zustral_test_" ".zum" in
  let contents =
    Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
        let channel = open_out path in
        for _ = 1 to line_count do output_string channel "a\n" done;
        close_out channel;
        read_file_to_string path)
  in
  eq (2 * line_count - 1) (String.length contents)

let suite =
  "Operating system" >::: [
      "run_command" >:: test_run_command;
      "exit_code" >:: test_exit_code;
      "read a file of a million lines" >:: test_read_file_of_a_million_lines
    ]

let _ = run_test_tt_main suite
