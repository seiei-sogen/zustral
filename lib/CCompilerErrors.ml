(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The errors of running the C compiler on the generated code, and their
    wording. *)
open ErrorText

type t =
  | CCompilerFailed of { command: string; exit_code: int; stdout: string; stderr: string }
  | PathVariableNotSet

let render (error: t): err_text =
  match error with
  | CCompilerFailed { command; exit_code; stdout; stderr } ->
     [
       Text "Failed to run C compiler.";
       Break;
       Text "Command: ";
       Code command;
       Break;
       Text "Exit code: ";
       Text (string_of_int exit_code);
       Break;
       Text "Standard output:\n";
       Text stdout;
       Break;
       Text "Standard error:\n";
       Text stderr
     ]
  | PathVariableNotSet ->
     [
       Text "The environment variable ";
       Code "PATH";
       Text " is not set, so the C compiler ";
       Code "cc";
       Text " cannot be found."
     ]
