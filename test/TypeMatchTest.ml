(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Zustral_core.Identifier
open Zustral_core.Common
open Zustral_core.EnvTypes
open Zustral_core.Error
open Zustral_core.Type
module Env = Zustral_core.Env
module TypeMatch = Zustral_core.TypeMatch
module InstanceIdSet = Zustral_core.InstanceIdSet.InstanceIdSet

(* No program reaches these: the compiler qualifies the name of a constraint
   against a module that exists (lib/CombiningPass.ml), and matches types from
   the module it is compiling. They hold try_match_type to its type: a module
   that is not in the environment is a failure it returns, like every other. *)

let example: module_name = make_mod_name "Example"

let nope: module_name = make_mod_name "Nope"

(* A type variable of the Free universe with one constraint, as the use of
   its declaration has it: matching binds it, and checks the constraint. *)
let constrained (name: string) (typeclass: sident): type_var =
  TypeVariable (make_ident name, FreeUniverse, make_qident (example, make_ident "f", make_ident "f"), [typeclass], BoundAtUse)

(* An environment with the module Example, and in it a type class C and an
   instance of C for Pointer[U], where U needs an instance of Nope.D. *)
let env_with_instance: Env.env * sident =
  let (env, mod_id) =
    Env.add_module Env.empty_env { name = example; imported_instances = InstanceIdSet.empty; imported_symbols = [] }
  in
  let typeclass: sident = make_sident example (make_ident "C") in
  let (env, typeclass_id) =
    Env.register NoSpan env (NewTypeClass {
        mod_id;
        vis = VisPublic;
        name = make_ident "C";
        docstring = Docstring "";
        param = Zustral_core.TypeParameter.make_typaram (make_ident "T", TypeUniverse, make_qident (example, make_ident "C", make_ident "C"), [])
      })
  in
  let (env, _) =
    Env.register NoSpan env (NewInstance {
        mod_id;
        vis = VisPublic;
        typeclass_id;
        docstring = Docstring "";
        typarams = Zustral_core.TypeParameters.empty_typarams;
        argument = Pointer (TyVar (constrained "U" (make_sident nope (make_ident "D"))))
      })
  in
  (env, typeclass)

let unknown_module (name: module_name): ('a, error) result =
  Error (Declaration (UnknownModule name))

let test_constraint_names_a_missing_module _ =
  let tv: type_var = constrained "T" (make_sident nope (make_ident "C")) in
  assert_equal (unknown_module nope) (TypeMatch.try_match_type (Env.empty_env, nope, NoSpan) (TyVar tv) Unit)

let test_match_from_a_missing_module _ =
  let (env, typeclass) = env_with_instance in
  let tv: type_var = constrained "T" typeclass in
  assert_equal (unknown_module nope) (TypeMatch.try_match_type (env, nope, NoSpan) (TyVar tv) Unit)

(* The lookup of an instance goes on to the next instance after every
   failure of a match. The failure here is the missing module of the
   instance's own constraint, so the instance is passed over and none is left. *)
let test_instance_with_a_missing_module_is_passed_over _ =
  let (env, typeclass) = env_with_instance in
  let tv: type_var = constrained "T" typeclass in
  assert_equal
    (Error (Type (UnsatisfiedConstraint { tyvar = tv; ty = Pointer Unit; typeclass })))
    (TypeMatch.try_match_type (env, example, NoSpan) (TyVar tv) (Pointer Unit))

(* A type parameter bound at use is not the same type as the one that stands
   for itself. The compiler compares the types of a body, whose type variables
   stand for themselves, and never two that are bound at use. *)
let test_bound_at_use_differs_from_standing_for_itself _ =
  let variable (matching: type_var_matching): ty =
    TyVar (TypeVariable (make_ident "T", FreeUniverse, make_qident (example, make_ident "f", make_ident "f"), [], matching))
  in
  assert_bool "a variable bound at use is itself" (equal_ty (variable BoundAtUse) (variable BoundAtUse));
  assert_bool "it is not the one that stands for itself" (not (equal_ty (variable BoundAtUse) (variable StandsForItself)))

let suite =
  "TypeMatch" >::: [
      "a constraint names a module that does not exist" >:: test_constraint_names_a_missing_module;
      "a match is asked from a module that does not exist" >:: test_match_from_a_missing_module;
      "an instance whose constraint names a missing module is passed over" >:: test_instance_with_a_missing_module_is_passed_over;
      "a type variable bound at use differs from the one that stands for itself" >:: test_bound_at_use_differs_from_standing_for_itself
    ]

let _ = run_test_tt_main suite
