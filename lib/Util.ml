(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Error

let string_explode (s: string): char list =
  List.init (String.length s) (String.get s)

let string_implode (l: char list): string =
  String.init (List.length l) (List.nth l)

let remove_char (s: string) (c: char)  =
  string_implode (List.filter (fun c' -> c <> c') (string_explode s))

let replace_char (s: string) (c: char) (r: string): string =
  let replace char =
    if char = c then
      string_explode r
    else
      [char]
  in
  string_implode (List.concat (List.map replace (string_explode s)))

type replacement = { text: string; search: string; replacement: string }

let search_replace (r: replacement): string =
  let { text: string; search: string; replacement: string } = r in
  let re = Str.regexp_string search in
  Str.global_replace re replacement text

let remove_leading (s: string) (n: int): string =
  String.sub s n ((String.length s) - n)

let parse_ascii_char (s: string): int =
  match s with
  | "\\n" ->
     10
  | "\\r" ->
     13
  | "\\t" ->
     9
  | "\\\\" ->
     92
  | _ ->
     (match (string_explode s) with
      | [c] ->
         Char.code c
      | _ ->
         parse_error NoSpan (LongCharacter s))

let rec count_leading_whitespace (s: string): int =
  let chars = string_explode s in
  match chars with
  | ' '::rest ->
     1 + (count_leading_whitespace (string_implode rest))
  | _ ->
     0

(* The lines of a string, as `String.split_on_char '\n'` gives them, as the
   first line and the others. A string has at least one line, and the type
   says so. *)
let split_lines (s: string): string * string list =
  let rec from (start: int): string * string list =
    match String.index_from_opt s start '\n' with
    | Some i ->
       let (next, rest) = from (i + 1) in
       (String.sub s start (i - start), next :: rest)
    | None ->
       (String.sub s start ((String.length s) - start), [])
  in
  from 0

let trim_common_whitespace (s: string): string =
  let (first, rest) = split_lines s in
  let min_whitespace: int =
    List.fold_left (fun m line -> min m (count_leading_whitespace line)) (count_leading_whitespace first) rest
  in
  let lines: string list = List.map (fun s -> remove_leading s min_whitespace) (first :: rest) in
  String.concat "\n" lines

let process_triple_string (s: string): string =
  let (first, rest) = split_lines s in
  let first = if (String.equal (String.trim first) "") then
                ""
              else
                first
  in
  match (List.rev rest) with
  | last::body ->
     let last = if (String.equal (String.trim last) "") then
                  ""
                else
                  last
     in
     let s = first ^ (String.concat "\n" (List.rev body)) ^ last in
     trim_common_whitespace s
  | [] ->
     first

let ident_set_eq a b =
  let sorter a b = compare (ident_string a) (ident_string b) in
  (List.sort sorter a) = (List.sort sorter b)

let rec map_with_context (f: ('c * 'a) -> ('c * 'b)) (ctx: 'c) (list: 'a list): ('c * ('b list)) =
  match list with
  | first::rest ->
     let (ctx', b) = f (ctx, first) in
     let (ctx'', rest') = map_with_context f ctx' rest in
     (ctx'', b :: rest')
  | [] ->
     (ctx, [])

let rec iter_with_context (f: ('c -> 'a -> 'c)) (ctx: 'c) (list: 'a list): 'c =
  match list with
  | first::rest ->
     let ctx' = f ctx first in
     iter_with_context f ctx' rest
  | [] ->
     ctx

let rec split_last (first: 'a) (rest: 'a list): 'a list * 'a =
  match rest with
  | second::rest ->
     let (init, last) = split_last second rest in
     (first :: init, last)
  | [] ->
     ([], first)
