(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Util
open Error

type escaped_string = EscapedString of string
[@@deriving show]

let rec escape_string (place: place) s =
  EscapedString (string_implode (escape_list place (string_explode s)))

and escape_list place lst =
  match lst with
  | ('\\' :: 'n' :: rest) -> '\n' :: (escape_list place rest)
  | ('\\' :: 'r' :: rest) -> '\r' :: (escape_list place rest)
  | ('\\' :: 't' :: rest) -> '\t' :: (escape_list place rest)
  | ('\\' :: '\'' :: rest) -> '\'' :: (escape_list place rest)
  | ('\\' :: '"' :: rest) -> '"' :: (escape_list place rest)
  | ('\\' :: '\\' :: rest) -> '\\' :: (escape_list place rest)
  | ('\\' :: ' ' :: rest) -> consume_whitespace place (' ' :: rest)
  | ('\\' :: '\n' :: rest) -> consume_whitespace place (' ' :: rest)
  | ('\\' :: '\r' :: rest) -> consume_whitespace place (' ' :: rest)
  | ('\\' :: '\t' :: rest) -> consume_whitespace place (' ' :: rest)
  | (head :: rest) -> head :: (escape_list place rest)
  | nil -> nil

and consume_whitespace place lst =
  match lst with
  | (' '  :: rest) -> consume_whitespace place rest
  | ('\n' :: rest) -> consume_whitespace place rest
  | ('\r' :: rest) -> consume_whitespace place rest
  | ('\t' :: rest) -> consume_whitespace place rest
  | ('\\' :: rest) -> rest
  | _ -> generic_error place BadWhitespaceEscape

let escaped_to_string (EscapedString s) =
  s

let rec unescape_string (EscapedString s) =
  String.concat "" (List.map unescape_char (string_explode s))

and unescape_char = function
  | '\n' -> "\\n"
  | '\r' -> "\\r"
  | '\t' -> "\\t"
  | '\'' -> "\\\'"
  | '\\' -> "\\\\"
  | '"' -> "\\\""
  | c -> String.make 1 c
