(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Id
open Identifier
open Type
open TypeBindings
open TypeParameters
open Stages.Tast
open Env
open EnvTypes

(** We need the environment and the module name to find instances of typeclasses
    in the module where the typechecking happens. This is used to check that,
    when matching a constrained type variable to a type, that the type
    implements the constraints.*)
type ctx = env * module_name * Error.place

val match_type_with_value : ctx -> ty -> texpr -> type_bindings

(** The two types are the same. For two types of the body being checked: a
    type parameter of the enclosing declaration stands for itself there, so
    nothing is bound. The error names the two whole types. *)
val expect_same_type : ctx -> ty -> ty -> unit

(** {!expect_same_type} for a value and the type its place declares. A literal
    that the rules for literals accept does not have to have the type. *)
val expect_value_of_type : ctx -> ty -> texpr -> unit

(** The bindings that make the first type the second. The first is a type of
    the signature of a declaration that is used
    ({!SignatureTypeAtUse.signature_type_at_use}): its type variables are
    bound to what the second type has in their place. A type variable that
    stands for itself ({!Type.StandsForItself}) is never bound: it matches
    itself and no other type. *)
val match_type : ctx -> ty -> ty -> type_bindings

(** [match_type] with a failure returned as a value, the error that reports
    it. For a caller that goes on when the types do not match. *)
val try_match_type : ctx -> ty -> ty -> (type_bindings, Error.error) result

val match_typarams : ctx -> typarams -> ty list -> type_bindings

(** Find an instance of the given typeclass given the dispatch type.

Parameters are: environment, source module name, dispatch type, and ID of the
typeclass. *)
val get_instance : Error.place -> env -> module_name -> ty -> typeclass_id -> (instance_decl * type_bindings) option

(** {!get_instance} among the instances of every module, also those the source
    module does not see. For a method call in a generic function, whose
    instance can be in the module that instantiates it (issue #128). The orphan
    rule leaves at most one instance that matches. *)
val get_instance_from_every_module : Error.place -> env -> module_name -> ty -> typeclass_id -> (instance_decl * type_bindings) option

(** The method of the given name of an instance found by {!get_instance}: a
    type error if the instance does not define it. *)
val get_method_of_instance : Error.place -> env -> instance_decl -> identifier -> ins_meth_id
