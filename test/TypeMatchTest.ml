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
open Austral_core.Type
module Env = Austral_core.Env
module TypeMatch = Austral_core.TypeMatch
module InstanceIdSet = Austral_core.InstanceIdSet.InstanceIdSet

(* No program reaches these: the compiler qualifies the name of a constraint
   against a module that exists (lib/CombiningPass.ml), and matches types from
   the module it is compiling. They hold try_match_type to its type: a module
   that is not in the environment is a failure it returns, like every other. *)

let example: module_name = make_mod_name "Example"

let nope: module_name = make_mod_name "Nope"

(* A type variable of the Free universe with one constraint. *)
let constrained (name: string) (typeclass: sident): type_var =
  TypeVariable (make_ident name, FreeUniverse, make_qident (example, make_ident "f", make_ident "f"), [typeclass])

(* An environment with the module Example, and in it a type class C and an
   instance of C for Pointer[U], where U needs an instance of Nope.D. *)
let env_with_instance: Env.env * sident =
  let (env, mod_id) =
    Env.add_module Env.empty_env { name = example; imported_instances = InstanceIdSet.empty }
  in
  let typeclass: sident = make_sident example (make_ident "C") in
  let (env, typeclass_id) =
    Env.register env (NewTypeClass {
        mod_id;
        vis = VisPublic;
        name = make_ident "C";
        docstring = Docstring "";
        param = Austral_core.TypeParameter.make_typaram (make_ident "T", TypeUniverse, make_qident (example, make_ident "C", make_ident "C"), [])
      })
  in
  let (env, _) =
    Env.register env (NewInstance {
        mod_id;
        vis = VisPublic;
        typeclass_id;
        docstring = Docstring "";
        typarams = Austral_core.TypeParameters.empty_typarams;
        argument = Pointer (TyVar (constrained "U" (make_sident nope (make_ident "D"))))
      })
  in
  (env, typeclass)

let unknown_module (name: module_name): ('a, error) result =
  Error (Declaration (UnknownModule name))

let test_constraint_names_a_missing_module _ =
  let tv: type_var = constrained "T" (make_sident nope (make_ident "C")) in
  assert_equal (unknown_module nope) (TypeMatch.try_match_type (Env.empty_env, nope) (TyVar tv) Unit)

let test_match_from_a_missing_module _ =
  let (env, typeclass) = env_with_instance in
  let tv: type_var = constrained "T" typeclass in
  assert_equal (unknown_module nope) (TypeMatch.try_match_type (env, nope) (TyVar tv) Unit)

(* The lookup of an instance goes on to the next instance after every
   failure of a match. The failure here is the missing module of the
   instance's own constraint, so the instance is passed over and none is left. *)
let test_instance_with_a_missing_module_is_passed_over _ =
  let (env, typeclass) = env_with_instance in
  let tv: type_var = constrained "T" typeclass in
  assert_equal
    (Error (Type (UnsatisfiedConstraint { tyvar = tv; ty = Pointer Unit; typeclass })))
    (TypeMatch.try_match_type (env, example) (TyVar tv) (Pointer Unit))

let suite =
  "TypeMatch" >::: [
      "a constraint names a module that does not exist" >:: test_constraint_names_a_missing_module;
      "a match is asked from a module that does not exist" >:: test_match_from_a_missing_module;
      "an instance whose constraint names a missing module is passed over" >:: test_instance_with_a_missing_module_is_passed_over
    ]

let _ = run_test_tt_main suite
