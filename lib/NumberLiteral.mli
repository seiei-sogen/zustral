(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** Number literals. The parser reads the text of a literal once, into one of
    these records, and every later stage carries the record. *)

(** How an integer literal was written. *)
type radix =
  | Decimal
  | Hexadecimal
  | Binary
  | Octal
  | Character

type int_literal = private {
  written: string;
  (** The text as the lexer saw it, with its prefix and digit separators. *)
  radix: radix;
  value: Z.t;
}

type float_literal = private {
  written: string;
  (** The text as the lexer saw it, with its digit separators, and a leading
      `-` when the literal is a negated one ([negated_float]). *)
  fraction_digits: int;
  (** How many digits follow the period. *)
  has_exponent: bool;
}

(** {1 Reading a token}

    One function per token kind. The argument is the token's text. *)

val of_decimal : string -> int_literal

val of_hexadecimal : string -> int_literal

val of_binary : string -> int_literal

val of_octal : string -> int_literal

(** The second argument is the character's code. The lexer works it out,
    because only it knows which escape the token is. *)
val of_character : string -> int -> int_literal

val of_float : string -> float_literal

(** An integer literal that nobody wrote: the value of a literal-only
    expression. Its text is the value in decimal. *)
val of_value : Z.t -> int_literal

(** The float literal of the negation of the literal: `-2.5` of `2.5`, and
    `2.5` of `-2.5`. *)
val negated_float : float_literal -> float_literal

(** {1 Printing} *)

(** The text the compiler prints for an integer literal in the message that
    says the literal does not fit a type. The generated C has its own text
    ([CIntegerConstant.text_of_value]). *)
val int_text : int_literal -> string

(** The text the compiler prints for a float literal into the generated C. *)
val float_text : float_literal -> string
