(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Type
open TypeVarSet

(* Get the universe this type belongs to *)
val type_universe : Error.place -> ty -> universe

(* Whether the type is a built-in integer or floating point type. *)
val is_numeric : Error.place -> ty -> bool

(* Return the set of type variables in a type expression, *)
val type_variables : ty -> TypeVarSet.t

val any_arg_is_linear : Error.place -> ty list -> bool

val any_arg_is_type : Error.place -> ty list -> bool
