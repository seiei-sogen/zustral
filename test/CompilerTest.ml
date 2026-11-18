(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Zustral_core
open Zustral_core.Identifier

(* The compiler numbers what it makes: declarations, instance methods,
   monomorphs, temporaries, the variables of the generated C, anonymous borrows
   and regions. Every compilation starts every numbering from the beginning,
   so compiling a program again in the same process gives the same names.
   These tests compile twice and compare. *)

let compile (code: string): CRepr.c_program Outcome.t =
  Compiler.compile
    [Pipeline.BodyModuleSource { body_filename = "Test.zum"; body_code = code }]
    (Some (make_mod_name "Test", make_ident "main"))

(* The generated C names a declaration (`decl_`), an instance method (`meth_`),
   monomorphs (`mono_`), temporaries (`_t`), C variables (`tmp`) and anonymous
   borrows (`_tmp_ref`) of this program. *)
let numbered_names: string = {|
module body Test is
    record Box: Linear is
        value: Int32;
    end;

    union Shape: Free is
        case Circle is
            radius: Int32;
        case Dot;
    end;

    typeclass Describe(T: Free) is
        method describe(value: T): Int32;
    end;

    instance Describe(Int32) is
        method describe(value: Int32): Int32 is
            return value;
        end;
    end;

    generic [R: Region]
    function peek(box: &[Box, R]): Int32 is
        return box->value;
    end;

    function main(): ExitCode is
        let box: Box := Box(value => 1);
        var total: Int32 := peek(&box);
        let shape: Shape := Circle(radius => total);
        case shape of
            when Circle(radius: Int32) do
                if radius > 0 then
                    total := describe(radius);
                end if;
            when Dot do
                skip;
        end case;
        let { value: Int32 } := box;
        return ExitSuccess();
    end;
end module body.
|}

let render (outcome: CRepr.c_program Outcome.t): string =
  match outcome with
  | Ok program -> CRenderer.render_program program
  | Error _ -> assert_failure "the program does not compile"

let test_same_c_twice _ =
  let first: string = render (compile numbered_names) in
  let second: string = render (compile numbered_names) in
  assert_equal ~msg:"the C of the second compilation is not the C of the first" first second

(* The part of the C that is the module Test. *)
let test_unit (c: string): string =
  let find (needle: string) (from: int): int =
    let n = String.length needle and len = String.length c in
    let rec go (i: int): int =
      if i + n > len then assert_failure ("the C has no `" ^ needle ^ "`")
      else if String.equal (String.sub c i n) needle then i
      else go (i + 1)
    in
    go from
  in
  let start: int = find "BEGIN translation unit for module 'Test'" 0 in
  String.sub c start (find "END translation unit for module 'Test'" start - start)

(* The identifiers of a text, in the order they appear. *)
let words (text: string): string list =
  let len = String.length text in
  let is_word_char (ch: char): bool =
    match ch with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> true
    | _ -> false
  in
  let rec word_end (i: int): int =
    if i < len && is_word_char text.[i] then word_end (i + 1) else i
  in
  let rec go (i: int) (found: string list): string list =
    if i >= len then List.rev found
    else if is_word_char text.[i] then
      let j = word_end i in
      go j (String.sub text i (j - i) :: found)
    else go (i + 1) found
  in
  go 0 []

(* Whether the word is a number from [i] to its end. *)
let number_from (word: string) (i: int): bool =
  i < String.length word
  && String.for_all (fun ch -> ch >= '0' && ch <= '9') (String.sub word i (String.length word - i))

let prefix_and_number (prefix: string) (word: string): bool =
  String.starts_with ~prefix word && number_from word (String.length prefix)

(* `<variable>_tmp_ref<n>`, the name of an anonymous borrow. *)
let borrow_name (word: string): bool =
  let marker = "_tmp_ref" in
  let n = String.length marker in
  let rec at (i: int): bool =
    i + n <= String.length word
    && ((String.equal (String.sub word i n) marker && number_from word (i + n)) || at (i + 1))
  in
  at 0

let rec first_appearances (seen: string list) (words: string list): string list =
  match words with
  | [] -> List.rev seen
  | word :: rest ->
     if List.mem word seen then first_appearances seen rest
     else first_appearances (word :: seen) rest

(* The numbers follow the order the stages visit the statements in, which is
   not the order of the source: the last statement of a block first. They go on
   from the numbers of the built-in modules, so a change to those modules
   shifts `_t128`, and this list is written again then. The base compiler of
   the refactoring gives the same names (compared when this test was written). *)
let test_numbers_in_order _ =
  let c: string list = words (test_unit (render (compile numbered_names))) in
  let numbered (word: string): bool =
    prefix_and_number "_t" word || prefix_and_number "tmp" word || borrow_name word
  in
  assert_equal ~printer:(String.concat " ")
    ["box_tmp_ref2"; "_t128"; "tmp2"; "_t129"; "_t130"; "tmp1"; "box_tmp_ref1"]
    (first_appearances [] (List.filter numbered c));
  List.iter
    (fun (prefix: string) ->
      assert_bool ("the C has no name that is `" ^ prefix ^ "` and a number") (List.exists (prefix_and_number prefix) c))
    ["decl_"; "meth_"; "mono_"]

(* The region of a borrow is numbered, and the number is in the error. *)
let numbered_region: string = {|
module body Test is
    function main(): ExitCode is
        let n: Int32 := 10;
        borrow ref: &[Int32, rho] := &n do
            let other: &[Int32, Static] := ref;
        end borrow;
        return ExitSuccess();
    end;
end module body.
|}

let test_same_error_twice _ =
  let errors (outcome: CRepr.c_program Outcome.t): Error.zustral_error list =
    match outcome with
    | Ok _ -> assert_failure "the program compiles"
    | Error (first, rest) -> first :: rest
  in
  let first: Error.zustral_error list = errors (compile numbered_region) in
  let second: Error.zustral_error list = errors (compile numbered_region) in
  assert_equal first second

(* A decimal constant of C from 2^63 up has no 64-bit signed type: gcc warns
   that it "is so large that it is unsigned", and the warning does not reach
   the user. The C of such a literal says its type itself (issue #157). *)
let test_64_bit_literals_in_c _ =
  let c: string = test_unit (render (compile {|
module body Test is
    function main(): ExitCode is
        let n: Nat64 := 18446744073709551615;
        let m: Int64 := -9223372036854775808;
        return ExitSuccess();
    end;
end module body.
|})) in
  let contains (part: string): bool =
    let n = String.length part in
    let rec at (i: int): bool =
      i + n <= String.length c && (String.equal (String.sub c i n) part || at (i + 1))
    in
    at 0
  in
  assert_bool "2^64 - 1 has the suffix U" (contains "18446744073709551615U");
  assert_bool "-2^63 is written as a subtraction" (contains "(-9223372036854775807 - 1)");
  assert_bool "2^63 is not a constant of the C" (not (contains "9223372036854775808"))

let suite =
  "Compiler" >::: [
      "the same program twice gives the same C" >:: test_same_c_twice;
      "the generated names have the numbers the stages hand out, in their order" >:: test_numbers_in_order;
      "the same failing program twice gives the same error" >:: test_same_error_twice;
      "a literal from 2^63 up, or -2^63, is a constant of C with no warning" >:: test_64_bit_literals_in_c
    ]

let _ = run_test_tt_main suite
