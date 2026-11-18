(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Zustral_core.Util

let test_map_with_context _ =
  let lst = ["a"; "b"; "c"; "d"]
  and ctx = 0
  and f ((c, a): (int * string)): (int * string) =
    (c + 1, String.uppercase_ascii a)
  in
  let (ctx', results) = map_with_context f ctx lst in
  assert_equal ctx' 4;
  assert_equal results ["A"; "B"; "C"; "D"]

let suite =
  "Utilities" >::: [
      "map_with_context" >:: test_map_with_context
    ]

let _ = run_test_tt_main suite
