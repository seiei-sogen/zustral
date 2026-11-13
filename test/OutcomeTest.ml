(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Austral_core
open Austral_core.Error

(* An error to fail with. Which one it is does not matter to these tests. *)
let error (message: string): austral_error =
  AustralError { location = Nowhere; error = Internal message; source_ctx = None }

let test_catch _ =
  assert_equal (Ok 1) (Outcome.catch (fun () -> 1));
  assert_equal (Error [error "a"]) (Outcome.catch (fun () -> internal_err NoSpan "a"));
  (* Only the compiler's own exception becomes a value. *)
  assert_raises Not_found (fun () -> Outcome.catch (fun () -> raise Not_found))

let test_traverse _ =
  let seen: int list ref = ref [] in
  let f (n: int): int Outcome.t =
    seen := n :: !seen;
    if n < 0 then
      Error [error (string_of_int n)]
    else
      Ok (n * 2)
  in
  assert_equal (Ok [2; 4; 6]) (Outcome.traverse f [1; 2; 3]);
  assert_equal [3; 2; 1] !seen;
  seen := [];
  (* The first failure is the result, and nothing after it is looked at. *)
  assert_equal (Error [error "-2"]) (Outcome.traverse f [1; -2; -3; 4]);
  assert_equal [-2; 1] !seen

let test_traverse_with_context _ =
  let f ((sum, n): (int * int)): (int * string) Outcome.t =
    if n < 0 then
      Error [error (string_of_int sum)]
    else
      Ok (sum + n, string_of_int n)
  in
  assert_equal (Ok (6, ["1"; "2"; "3"])) (Outcome.traverse_with_context f 0 [1; 2; 3]);
  (* The step that fails was given the context of the steps before it. *)
  assert_equal (Error [error "3"]) (Outcome.traverse_with_context f 0 [1; 2; -1; 4])

let suite =
  "Outcome" >::: [
      "catch" >:: test_catch;
      "traverse" >:: test_traverse;
      "traverse_with_context" >:: test_traverse_with_context
    ]

let _ = run_test_tt_main suite
