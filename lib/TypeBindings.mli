(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** This module defines a type for type bindings: a map from type parameters to
    types. *)
open Type
open TypeParameter

(** A type bindings value represents a map from type parameters to actual
    types. These are created as part of the type checking process. *)
type type_bindings

val bindings_list : type_bindings -> (type_parameter * ty) list

val empty_bindings : type_bindings

val bindings_from_list : Error.place -> (type_parameter * ty) list -> type_bindings

val merge_bindings : Error.place -> type_bindings -> type_bindings -> type_bindings

(** [merge_bindings], and adding one binding, with a failure returned as a
    value, the error that reports it. *)
val try_merge_bindings : type_bindings -> type_bindings -> (type_bindings, Error.error) result

val try_add_binding : type_bindings -> type_parameter -> ty -> (type_bindings, Error.error) result

val get_binding : type_bindings -> type_parameter -> ty option

(* Given a set of bindings, and a type expression, replace every type variable
   in the expression with the value from the bindings. If the variable is not
   found in the bindings, do nothing. A binding is of a type parameter, so it
   replaces the parameter's variable whether that stands for itself or is
   bound at use. The types put in are not searched for variables again. *)
val replace_variables : type_bindings -> ty -> ty

(** The universe of a named type of that universe whose arguments are these,
    once their type variables are replaced: a type of the `Type` universe
    takes the universe of its arguments. *)
val replaced_universe : universe -> ty list -> universe

val replace_region_variables : type_bindings -> ref_region -> ref_region

(** A type of the Region universe as the region of a reference or a span. An
    internal error for any other type. *)
val ref_region_of_region_type : ty -> ref_region
