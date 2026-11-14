(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Stores the lexical environment. *)
open Identifier
open Common
open Type

(** A lexical environment: the variables in scope at a point of a function
    body. It is a value. Adding or removing a variable gives another
    environment, and the one it was given stays as it was. *)
type lexenv

(** Where a variable of the lexical environment comes from: a function
    parameter, or a local variable. *)
type var_source =
  | VarParam
  | VarLocal of mutability

(** What a name used as a variable refers to: a constant, which the
    environment holds and the lexical environment does not, or a variable of
    the lexical environment. *)
type variable_source =
  | VarConstant
  | VarLexical of var_source

(** The empty lexenv. *)
val empty_lexenv : lexenv

(** The lexenv with one more variable, given its name, type, and source. If a
    variable with that name is in it already, this raises the error that says
    so: a program that declares a name twice in one scope gets here. *)
val push_var : Error.place -> lexenv -> identifier -> ty -> var_source -> lexenv

(** Add a list of variables. *)
val push_vars : Error.place -> lexenv -> (identifier * ty * var_source) list -> lexenv

(** The lexenv without the variable of that name. *)
val remove_var : lexenv -> identifier -> lexenv

val remove_vars : lexenv -> identifier list -> lexenv

(** Retrieve a variable by name. *)
val get_var : lexenv -> identifier -> (ty * var_source) option

(** The type of a temporary, given its name. The temporary must be in the
    lexenv: the passes that make temporaries declare each before they use it. *)
val get_temporary_type : Error.place -> lexenv -> identifier -> ty
