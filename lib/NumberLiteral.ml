(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

type radix =
  | Decimal
  | Hexadecimal
  | Binary
  | Octal
  | Character

type int_literal = {
  written: string;
  radix: radix;
  value: Z.t;
}

type float_literal = {
  written: string;
  fraction_digits: int;
  has_exponent: bool;
}

(* This module depends on nothing else in the compiler: the syntax tree carries
   its records, and the modules that describe errors carry the syntax tree. *)

let without_separators (written: string): string =
  String.concat "" (String.split_on_char '\'' written)

(* The digits after a `#x`, `#b` or `#o` prefix, read in that radix. The lexer
   admits only digits of the radix and separators, so this cannot fail. *)
let value_in_radix (zarith_prefix: string) (written: string): Z.t =
  let unseparated: string = without_separators written in
  let digits: string = String.sub unseparated 2 (String.length unseparated - 2) in
  Z.of_string (zarith_prefix ^ digits)

let of_decimal (written: string): int_literal =
  (* The lexer admits digits and separators, all of which `Z.of_string` reads
     once the separators are gone, so this cannot fail. *)
  { written; radix = Decimal; value = Z.of_string (without_separators written) }

let of_hexadecimal (written: string): int_literal =
  { written; radix = Hexadecimal; value = value_in_radix "0x" written }

let of_binary (written: string): int_literal =
  { written; radix = Binary; value = value_in_radix "0b" written }

let of_octal (written: string): int_literal =
  { written; radix = Octal; value = value_in_radix "0o" written }

let of_character (written: string) (code: int): int_literal =
  { written; radix = Character; value = Z.of_int code }

let of_float (written: string): float_literal =
  let length: int = String.length written in
  let exponent_start: int =
    match (String.index_opt written 'e', String.index_opt written 'E') with
    | (Some i, _) | (None, Some i) -> i
    | (None, None) -> length
  in
  (* The lexer's float token always has a period. *)
  let period: int = String.index written '.' in
  let fraction: string = String.sub written (period + 1) (exponent_start - period - 1) in
  let count_digit (n: int) (c: char): int =
    if '0' <= c && c <= '9' then n + 1 else n
  in
  { written; fraction_digits = String.fold_left count_digit 0 fraction; has_exponent = exponent_start < length }

let of_value (value: Z.t): int_literal =
  { written = Z.to_string value; radix = Decimal; value }

let negated_float (literal: float_literal): float_literal =
  let length: int = String.length literal.written in
  if String.starts_with ~prefix:"-" literal.written then
    { literal with written = String.sub literal.written 1 (length - 1) }
  else
    { literal with written = "-" ^ literal.written }

(* The value in decimal, whatever the literal looked like. *)
let int_text (literal: int_literal): string =
  Z.to_string literal.value

let float_text (literal: float_literal): string =
  without_separators literal.written
