(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Austral_core.Identifier
open Austral_core.Common
open Austral_core.EnvTypes
open Austral_core.Error
module Env = Austral_core.Env
module Id = Austral_core.Id
module DeclIdSet = Austral_core.DeclIdSet.DeclIdSet
module ModIdSet = Austral_core.ModIdSet.ModIdSet

(* The message of the internal error the function raises, if it raises one. *)
let internal_error_of (f: unit -> 'a): string option =
  match f () with
  | _ -> None
  | exception Austral_error (AustralError { error = Internal message; _ }) -> Some message

(* No program reaches this: an ID is in the environment that made it and in
   every later one. Asking an earlier environment is the one way a lookup by
   ID can miss, and the miss is reported inside lib/Env.ml. *)
let test_id_from_a_later_environment _ =
  let (before, mod_id) =
    Env.add_module Env.empty_env {
        name = make_mod_name "Example";
        imported_instances = DeclIdSet.empty;
        imports_from = ModIdSet.empty
      }
  in
  let (after, id) =
    Env.register before (NewConstant {
        mod_id;
        vis = VisPublic;
        name = make_ident "example";
        ty = Austral_core.Type.Unit;
        docstring = Docstring ""
      })
  in
  assert_bool "the environment that made the ID has the declaration"
    (Id.equal_decl_id id (Austral_core.EnvUtils.decl_id (Env.get_decl_by_id after id)));
  assert_equal
    (Some ("No declaration with ID `" ^ (Id.show_decl_id id) ^ "` in this environment."))
    (internal_error_of (fun () -> Env.get_decl_by_id before id))

let suite =
  "Env" >::: [
      "an ID from a later environment" >:: test_id_from_a_later_environment
    ]

let _ = run_test_tt_main suite
