(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(* The wording of errors that no program makes the compiler raise, and the
   parts of the error printer and the command line that no program reaches. *)
open OUnit2
open Austral_core
open Austral_core.Identifier
open Austral_core.Error
open Austral_core.SourceContext
open TestUtil

let wording (error: error): string =
  ErrorText.error_text_to_plain (error_text error)

let raised_wording (f: unit -> 'a): string =
  try
    ignore (f ());
    assert_failure "No error was raised."
  with Austral_error (AustralError { error; _ }) ->
    error_title error ^ ": " ^ wording error

let test_generic_wording _ =
  let open GenericErrors in
  eq "Cannot add symbol to the import map twice." (wording (Generic SymbolImportedTwice));
  eq "Parameter not found." (wording (Generic ParameterNotFound));
  eq "Not allowed" (wording (Generic InstanceOverlapTypeNotAllowed))

let test_declaration_wording _ =
  let open DeclarationErrors in
  eq "The interface requires `f` to be a instance"
    (wording (Declaration (DeclarationKindMismatch { name = i "f"; expected = InstanceDeclaration })));
  eq "Typeclass arguments cannot be regions." (wording (Declaration InstanceArgumentRegion))

let test_parse_wording _ =
  eq "Character not allowed in string literal: `\\x00`"
    (wording (Parse (ParseErrors.StringInvalidCharacter "\\x00")))

let test_empty_argument_list _ =
  eq "Command Line Arguments Error: Argument list must have at least one element in it, the path to the binary. Was the compiler invoked through a system call?"
    (raised_wording (fun () -> Austral_cli.CliUtil.parse_args []))

let test_popping_flag_of_other_kind _ =
  let open Austral_cli.CliUtil in
  let arglist = parse_args ["bin"; "pos"; "--a=1"; "--b"] in
  eq "Command Line Arguments Error: Expected `a` to be a boolean flag, but it was given as a value flag."
    (raised_wording (fun () -> pop_bool_flag arglist "a"));
  eq "Command Line Arguments Error: Expected `b` to be a value flag, but it was given as a boolean flag."
    (raised_wording (fun () -> pop_value_flag arglist "b"))

let test_write_error_wording _ =
  eq "Failed to write to file `out.c`: \n\nIs a directory"
    (wording (Cli (CliErrors.WriteError { path = "out.c"; reason = "Is a directory" })))

let test_write_to_full_device _ =
  eq "Command Line Arguments Error: Failed to write to file `/dev/full`: \n\nNo space left on device"
    (raised_wording (fun () -> Austral_cli.Os.write_string_to_file "/dev/full" "x"))

let test_command_killed_by_signal _ =
  let (Austral_cli.Os.CommandOutput { code; _ }) = Austral_cli.Os.run_command "kill -9 $$" in
  eq Sys.sigkill code

let module_of (error: austral_error): string option =
  let (AustralError { location; _ }) = error in
  Option.map mod_name_string (location_module_name location)

let test_module_name_is_added_once _ =
  let first = make_mod_name "First" and second = make_mod_name "Second" in
  let without_span = add_module_name (make_error NoSpan (Internal "x")) first in
  eq (Some "First") (module_of (add_module_name without_span second));
  let with_span = add_module_name (make_error (AtSpan (simplespan "x")) (Internal "x")) first in
  eq (Some "First") (module_of (add_module_name with_span second))

let source_ctx_of (error: austral_error): source_ctx option =
  let (AustralError { source_ctx; _ }) = error in
  source_ctx

let test_source_context_is_added_once _ =
  let first = SourceContext [(1, "first")] and second = SourceContext [(1, "second")] in
  let error = add_source_ctx (make_error NoSpan (Internal "x")) first in
  eq (Some first) (source_ctx_of (add_source_ctx error second))

let test_line_numbers_are_right_aligned _ =
  eq " 9 | a\n10 | b" (source_ctx_to_plain_text (SourceContext [(9, "a"); (10, "b")]))

let test_html_of_context_without_span _ =
  let error = add_source_ctx (make_error NoSpan (Internal "x")) (SourceContext [(3, "let x")]) in
  let html = HtmlError.render_error_to_html error in
  let contains (sub: string): bool =
    let n = String.length sub in
    let rec from k = k + n <= String.length html && (String.sub html k n = sub || from (k + 1)) in
    from 0
  in
  eq true (contains "<pre>let x<br></pre>");
  eq false (contains "<span class='error-hl'>")

let suite =
  "Unexecuted wording" >::: [
      "Generic errors" >:: test_generic_wording;
      "Declaration errors" >:: test_declaration_wording;
      "Parse errors" >:: test_parse_wording;
      "Empty argument list" >:: test_empty_argument_list;
      "Popping a flag of the other kind" >:: test_popping_flag_of_other_kind;
      "Write error" >:: test_write_error_wording;
      "Write to a full device" >:: test_write_to_full_device;
      "Command killed by a signal" >:: test_command_killed_by_signal;
      "Module name is added once" >:: test_module_name_is_added_once;
      "Source context is added once" >:: test_source_context_is_added_once;
      "Line numbers are right aligned" >:: test_line_numbers_are_right_aligned;
      "HTML of a context without a span" >:: test_html_of_context_without_span
    ]

let _ = run_test_tt_main suite
