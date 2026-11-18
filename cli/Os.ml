(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Zustral_core
open Unix
open Error

(* The message of [Sys_error] starts with the path when the file cannot be
   opened, and not when it cannot be read or written. The error names the
   path itself. *)
let reason_without_path (path: string) (reason: string): string =
  let prefix = path ^ ": " in
  if String.starts_with ~prefix reason then
    String.sub reason (String.length prefix) (String.length reason - String.length prefix)
  else
    reason

(* Reads in a loop, not by recursion per line: a long file must not use up the
   stack (#93). In_channel.input_all would do, but it needs OCaml 4.14, and
   the build on macOS uses 4.13. *)
let read_stream_to_string (stream: in_channel): string =
  let contents = Buffer.create 65536 in
  let chunk = Bytes.create 65536 in
  let rec read_chunks () =
    let length = input stream chunk 0 (Bytes.length chunk) in
    if length > 0 then begin
      Buffer.add_subbytes contents chunk 0 length;
      read_chunks ()
    end
  in
  read_chunks ();
  String.trim (Buffer.contents contents)

let read_file_to_string (path: string): string =
  try
    let stream = open_in path in
    Fun.protect ~finally:(fun () -> close_in_noerr stream) (fun () -> read_stream_to_string stream)
  with Sys_error reason ->
    cli_error NoSpan (ReadError { path; reason = reason_without_path path reason })

let write_string_to_file (path: string) (contents: string): unit =
  try
    let stream = open_out path in
    Fun.protect ~finally:(fun () -> close_out_noerr stream) (fun () ->
        output_string stream contents;
        (* Closing flushes, and close_out_noerr would close without reporting
           a failure (a full disk). *)
        close_out stream)
  with Sys_error reason ->
    cli_error NoSpan (WriteError { path; reason = reason_without_path path reason })

type command_output =
  CommandOutput of { command: string; code: int; stdout: string; stderr: string }

let run_command (command: string): command_output =
  let path: string =
    match Sys.getenv_opt "PATH" with
    | Some path -> path
    | None -> c_compiler_error NoSpan PathVariableNotSet
  in
  let (stdout_chan, stdin_chan, stderr_chan) = open_process_full command [|"PATH=" ^ path|] in
  let stdout = read_stream_to_string stdout_chan
  and stderr = read_stream_to_string stderr_chan in
  let proc_stat = close_process_full (stdout_chan, stdin_chan, stderr_chan) in
  CommandOutput {
      command = command;
      code = ProcessStatus.exit_code proc_stat;
      stdout = stdout;
      stderr = stderr
    }

let compile_c_code (source_path: string) (output_path: string): command_output =
  let cmd = "cc " ^ source_path ^ " -fwrapv -lm -o " ^ output_path in
  let o = run_command cmd in
  let (CommandOutput { command; code; stdout; stderr }) = o in
  if code <> 0 then
    c_compiler_error NoSpan (CCompilerFailed { command; exit_code = code; stdout; stderr })
  else
    o
