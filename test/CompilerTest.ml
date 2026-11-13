(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Austral_core
open Austral_core.Identifier

(* The compiler numbers what it makes: declarations, instance methods,
   monomorphs, temporaries, the variables of the generated C, anonymous borrows
   and regions. Every compilation starts every numbering from the beginning,
   so compiling a program again in the same process gives the same names.
   These tests compile twice and compare. *)

let compile (code: string): CRepr.c_program Outcome.t =
  Compiler.compile
    [Compiler.BodyModuleSource { body_filename = "Test.aum"; body_code = code }]
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

let test_same_c_twice _ =
  let render (outcome: CRepr.c_program Outcome.t): string =
    match outcome with
    | Ok program -> CRenderer.render_program program
    | Error _ -> assert_failure "the program does not compile"
  in
  let first: string = render (compile numbered_names) in
  let second: string = render (compile numbered_names) in
  assert_equal ~msg:"the C of the second compilation is not the C of the first" first second

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
  let errors (outcome: CRepr.c_program Outcome.t): Error.austral_error list =
    match outcome with
    | Ok _ -> assert_failure "the program compiles"
    | Error errors -> errors
  in
  let first: Error.austral_error list = errors (compile numbered_region) in
  let second: Error.austral_error list = errors (compile numbered_region) in
  assert_equal first second

let suite =
  "Compiler" >::: [
      "the same program twice gives the same C" >:: test_same_c_twice;
      "the same failing program twice gives the same error" >:: test_same_error_twice
    ]

let _ = run_test_tt_main suite
