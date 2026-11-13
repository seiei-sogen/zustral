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
module InstanceIdSet = Austral_core.InstanceIdSet.InstanceIdSet

(* The message of the internal error the function raises, if it raises one. *)
let internal_error_of (f: unit -> 'a): string option =
  match f () with
  | _ -> None
  | exception Austral_error (AustralError { error = Internal message; _ }) -> Some message

let register_function (env: Env.env) (mod_id: Id.mod_id) (name: string): Env.env * Id.function_id =
  Env.register NoSpan env (NewFunction {
      mod_id;
      vis = VisPublic;
      name = make_ident name;
      docstring = Docstring "";
      typarams = Austral_core.TypeParameters.empty_typarams;
      value_params = [];
      rt = Austral_core.Type.Unit;
      impl = BodyNotAttached
    })

let add_module (env: Env.env) (name: string): Env.env * Id.mod_id =
  Env.add_module env {
      name = make_mod_name name;
      imported_instances = InstanceIdSet.empty
    }

(* No program reaches this: an ID is in the environment that made it and in
   every later one. Asking an earlier environment is the one way a lookup by
   ID can miss, and the miss is reported inside lib/Env.ml. *)
let test_id_from_a_later_environment _ =
  let (before, mod_id) = add_module Env.empty_env "Example" in
  let (after, id) = register_function before mod_id "example" in
  assert_bool "the environment that made the ID has the function"
    (Id.equal_function_id id (Env.get_function after id).id);
  assert_equal
    (Some ("No function with ID `" ^ (Id.show_function_id id) ^ "` in this environment."))
    (internal_error_of (fun () -> Env.get_function before id))

(* A limit, not a check: an ID's type says which kind of declaration it names,
   not which environment made it. Two environments built separately from
   `Env.empty_env` give out the same numbers, so an ID of one finds a
   declaration of the other, with no error. A compilation has one lineage of
   environments (the driver hands each stage the one before), so no program
   reaches this. *)
let test_id_from_another_lineage _ =
  let (env_a, mod_a) = add_module Env.empty_env "A" in
  let (env_a, id_a) = register_function env_a mod_a "from_a" in
  let (env_b, mod_b) = add_module Env.empty_env "B" in
  let (_, id_b) = register_function env_b mod_b "from_b" in
  assert_bool "the two lineages give the same number"
    (Id.equal_function_id id_a id_b);
  assert_equal ~printer:ident_string (make_ident "from_a")
    (Env.get_function env_a id_b).name

(* No program reaches this either. A monomorph is registered under a key: what
   it is a monomorph of, and the type arguments. A key is not an ID, so an
   environment can have the type and not the monomorph. Here nothing has
   monomorphized the entrypoint, and the stage that asks for the monomorph of
   its return type reports the miss and names the type. *)
let test_monomorph_key_not_registered _ =
  let pervasive = Austral_core.BuiltIn.pervasive_module_name
  and exit_code = make_ident Austral_core.BuiltInNames.exit_code_name
  and main = make_ident "main" in
  let (env, mod_id) =
    Env.add_module Env.empty_env {
        name = pervasive;
        imported_instances = InstanceIdSet.empty
      }
  in
  let (env, _) =
    Env.register NoSpan env (NewUnion {
        mod_id;
        vis = TypeVisPublic;
        name = exit_code;
        docstring = Docstring "";
        typarams = Austral_core.TypeParameters.empty_typarams;
        universe = Austral_core.Type.FreeUniverse
      })
  in
  let (env, _) =
    Env.register NoSpan env (NewFunction {
        mod_id;
        vis = VisPublic;
        name = main;
        docstring = Docstring "";
        typarams = Austral_core.TypeParameters.empty_typarams;
        value_params = [];
        rt = Austral_core.Type.NamedType (make_qident (pervasive, exit_code, exit_code), [], Austral_core.Type.FreeUniverse);
        impl = BodyNotAttached
      })
  in
  assert_equal
    (Error [make_error NoSpan (Internal "No monomorph of ExitCode.")])
    (Austral_core.Entrypoint.entrypoint_code env (make_qident (pervasive, main, main)))

let suite =
  "Env" >::: [
      "an ID from a later environment" >:: test_id_from_a_later_environment;
      "an ID from another lineage of environments" >:: test_id_from_another_lineage;
      "a monomorph whose key is not registered" >:: test_monomorph_key_not_registered
    ]

let _ = run_test_tt_main suite
