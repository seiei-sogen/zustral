(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
(*open Austral_core.Common*)
open Austral_core.Cst
open Austral_core.NumberLiteral
open Austral_core.ParserInterface
open TestUtil

let p = parse_expr

let peq (s: string) (v: 'a) =
  eq v (p s)

let test_parse_nil_constant _ =
  let s = "nil" in
  peq s (CNilConstant (simplespan s))

let test_parse_bool_constant _ =
  let s = "true" in
  peq s (CBoolConstant (simplespan s, true));
  let s = "false" in
  peq s (CBoolConstant (simplespan s, false))

(* Only NumberLiteral builds a literal's record, so these two tests read the
   fields of the one the parser returns. *)
let test_parse_int_constant _ =
  let check (written: string) (radix: radix) (value: int) =
    match p written with
    | CIntConstant (span, literal) ->
       eq (simplespan written) span;
       eq written literal.written;
       eq radix literal.radix;
       eq ~cmp:Z.equal (Z.of_int value) literal.value
    | _ ->
       assert_failure (written ^ " did not parse as an integer constant")
  in
  check "0" Decimal 0;
  check "123" Decimal 123;
  check "1'000" Decimal 1000;
  check "010" Decimal 10;
  check "#xFF'FF" Hexadecimal 65535;
  check "#b11000" Binary 24;
  check "#o30" Octal 24;
  check "'a'" Character 97

let test_parse_float_constant _ =
  let check (written: string) (fraction_digits: int) (has_exponent: bool) =
    match p written with
    | CFloatConstant (span, literal) ->
       eq (simplespan written) span;
       eq written literal.written;
       eq fraction_digits literal.fraction_digits;
       eq has_exponent literal.has_exponent
    | _ ->
       assert_failure (written ^ " did not parse as a float constant")
  in
  check "0.0" 1 false;
  check "123.3" 1 false;
  check "1." 0 false;
  check "1'000.0e12" 1 true;
  check "1'000.000'1E-3" 4 true

let test_number_literal_text _ =
  eq "1000" (int_text (of_decimal "1'000"));
  eq "010" (int_text (of_decimal "010"));
  eq "65535" (int_text (of_hexadecimal "#xFF'FF"));
  eq "97" (int_text (of_character "'a'" 97));
  eq "-3" (int_text (of_value (Z.of_int (-3))));
  eq "1000.0001e-3" (float_text (of_float "1'000.000'1e-3"))

(*let test_parse_string_constant _ =
  let s = "\"test\"" in
  peq s (CStringConstant (simplespan s, "test"))
  let s = "\"test test\"" in
  peq s (CStringConstant (simplespan s, "test test"));
  let s = "\"test \\\"test\\\" test\"" in
  peq s (CStringConstant (simplespan s, "test \"test\" test"))*)

let test_parse_variable _ =
  let s = "a" in
  peq s (CVariable (simplespan s, i s));
  let s = "A" in
  peq s (CVariable (simplespan s, i s));
  let s = "Test" in
  peq s (CVariable (simplespan s, i s));
  let s = "Test_Test" in
  peq s (CVariable (simplespan s, i s));
  let s = "Test_Test3" in
  peq s (CVariable (simplespan s, i s))

let test_parse_funcall_positional _ =
  let s = "f(true)" in
  peq s (CFuncall (simplespan s, i "f", ConcretePositionalArgs [CBoolConstant (span 1 2 1 6, true)]));
  let s = "f(true, nil)" in
  peq s (CFuncall (simplespan s, i "f", ConcretePositionalArgs [CBoolConstant (span 1 2 1 6, true); (CNilConstant (span 1 8 1 11))]))

(*let test_parse_funcall_named _ =
  let s = "f(a => true)" in
  peq s (CFuncall (simplespan s, i "f", ConcreteNamedArgs [(i "a", CBoolConstant true)]));
  let s = "f(a => true, b => nil)" in
  peq s (CFuncall (simplespan s, i "f", ConcreteNamedArgs [(i "a", CBoolConstant true); (i "b", CNilConstant)]))

let test_parse_arithmetic _ =
  let add a b = CArith (Add, a, b)
  and sub a b = CArith (Subtract, a, b)
  and mul a b = CArith (Multiply, a, b)
  and div a b = CArith (Divide, a, b)
  and v s = CVariable (i s)
  in
  peq "a" (v "a");
  peq "a+b" (add (v "a") (v "b"));
  peq "a-b" (sub (v "a") (v "b"));
  peq "a*b" (mul (v "a") (v "b"));
  peq "a/b" (div (v "a") (v "b"));
  peq "(a-c)*c + x" (add (mul (sub (v "a") (v "c")) (v "c")) (v "x"))
 *)
let suite =
  "Expression parser" >::: [
      "Nil constant" >:: test_parse_nil_constant;
      "Bool constant" >:: test_parse_bool_constant;
      "Int constant" >:: test_parse_int_constant;
      "Float constant" >:: test_parse_float_constant;
      "Number literal text" >:: test_number_literal_text;
      (*      "String constant" >:: test_parse_string_constant;*)
      "Variable" >:: test_parse_variable;
      "Function call, positional arguments" >:: test_parse_funcall_positional
      (*"Function call, named arguments" >:: test_parse_funcall_named;
      "Arithmetic" >:: test_parse_arithmetic*)
    ]

let _ = run_test_tt_main suite
