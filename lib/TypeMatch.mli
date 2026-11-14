(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
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

val match_type : ctx -> ty -> ty -> type_bindings

(** [match_type] with a failure returned as a value, the error that reports
    it. For a caller that goes on when the types do not match. *)
val try_match_type : ctx -> ty -> ty -> (type_bindings, Error.error) result

val match_typarams : ctx -> typarams -> ty list -> type_bindings

(** Find an instance of the given typeclass given the dispatch type.

Parameters are: environment, source module name, dispatch type, and ID of the
typeclass. *)
val get_instance : Error.place -> env -> module_name -> ty -> typeclass_id -> (instance_decl * type_bindings) option

(** The method of the given name of an instance found by {!get_instance}: a
    type error if the instance does not define it. *)
val get_method_of_instance : Error.place -> env -> instance_decl -> identifier -> ins_meth_id
