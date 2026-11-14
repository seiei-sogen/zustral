(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Austral_core.BuiltInNames

(* This type and the constructors after it, in the order of the definition.
   The match has no wildcard arm, so a new constructor of [builtin_type] does
   not build until it has an arm here, and its arm puts it in the list. *)
let rec this_and_later (t: builtin_type): builtin_type list =
  t :: (match t with
        | Unit -> this_and_later Bool
        | Bool -> this_and_later Nat8
        | Nat8 -> this_and_later Nat16
        | Nat16 -> this_and_later Nat32
        | Nat32 -> this_and_later Nat64
        | Nat64 -> this_and_later Int8
        | Int8 -> this_and_later Int16
        | Int16 -> this_and_later Int32
        | Int32 -> this_and_later Int64
        | Int64 -> this_and_later ByteSize
        | ByteSize -> this_and_later Index
        | Index -> this_and_later Float32
        | Float32 -> this_and_later Float64
        | Float64 -> this_and_later Static
        | Static -> this_and_later Fn
        | Fn -> [])

(* [type_of_name] searches a list that is kept by hand. A type missing from
   that list is one the type parser never recognises. *)
let test_every_type_is_found_by_its_name _ =
  let name_of (found: builtin_type option): string =
    match found with
    | Some t -> type_name t
    | None -> "no type"
  in
  List.iter
    (fun (t: builtin_type) ->
      assert_equal ~printer:name_of (Some t) (type_of_name (type_name t)))
    (this_and_later Unit)

let suite =
  "BuiltInNames" >::: [
      "every built-in type is found by its name" >:: test_every_type_is_found_by_its_name
    ]

let _ = run_test_tt_main suite
