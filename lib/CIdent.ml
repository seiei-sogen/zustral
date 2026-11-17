(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

type t = {
    name: string;
    (* Whether the reserved-word escape applies when the name is rendered. *)
    escape: bool;
  }

let generated (name: string): t =
  { name; escape = false }

let zustral (name: string): t =
  { name; escape = true }

let external_name (name: string): t =
  { name; escape = false }

(* https://en.cppreference.com/w/c/keyword, and asm and fortran, which C
   compilers reserve. *)
let c_reserved_words: string list = [
    "alignas"; "alignof"; "auto"; "bool"; "break"; "case"; "char"; "const"; "constexpr";
    "continue"; "default"; "do"; "double"; "else"; "enum"; "extern"; "false"; "float";
    "for"; "goto"; "if"; "inline"; "int"; "long"; "nullptr"; "register"; "restrict";
    "return"; "short"; "signed"; "sizeof"; "static"; "static_assert"; "struct"; "switch";
    "thread_local"; "true"; "typedef"; "typeof"; "typeof_unqual"; "union"; "unsigned";
    "void"; "volatile"; "while"; "_Alignas"; "_Alignof"; "_Atomic"; "_BitInt"; "_Bool";
    "_Complex"; "_Decimal128"; "_Decimal32"; "_Decimal64"; "_Generic"; "_Imaginary";
    "_Noreturn"; "_Static_assert"; "_Thread_local"; "_Pragma"; "asm"; "fortran"
  ]

let is_c_reserved_word (name: string): bool =
  List.mem name c_reserved_words

let render (ident: t): string =
  if ident.escape && is_c_reserved_word ident.name then
    "au_ckw_" ^ ident.name
  else
    ident.name

let equal (a: t) (b: t): bool =
  String.equal (render a) (render b)
