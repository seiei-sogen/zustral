(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Zustral_core.Identifier
open Zustral_core.Span

let eq = assert_equal
let i = make_ident

let span l c l' c' =
  Span {
      filename = "";
      startp = Position {
                   line = l;
                   column = c;
                 };
      endp = Position {
                 line = l';
                 column = c';
               };
  }

let simplespan s =
  span 1 0 1 (String.length s)
