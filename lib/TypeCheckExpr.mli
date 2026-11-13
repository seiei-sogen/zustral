(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)
(** Type checking of expressions. *)
open Stages.AstDB
open Stages.Tast
open Type
open Identifier
open Env
open RegionMap
open TypeParameters
open LexEnv
open TypeBindings

(** The type checking context. *)
type expr_ctx

val make_ctx : module_name -> env -> region_map -> typarams -> lexenv -> expr_ctx

(** Type check an expression that stands where nothing says what its type is:
    an operand, an argument, a condition, the value of a `let` without the
    type it is checked against. The type of the result is the type of the
    expression. A generic function used as a value, and a call whose return
    type has a type variable that none of its parameters mentions, are
    errors here. *)
val infer : expr_ctx -> aexpr -> texpr

(** Type check an expression that stands where a type is expected: the value
    of a `return`, of an assignment, of a constant. The expected type fixes
    the type variables that the expression leaves open, and the type of the
    result has to match it. An integer literal has to fit it instead, and
    stays a literal. *)
val check : expr_ctx -> aexpr -> ty -> texpr

(** Type check an expression under a type annotation: `e : T`, and the value
    of `let x: T := e`. The annotated type is the expected type of the
    expression, as in [check], and an annotation does more than a place that
    expects a type:

    - it gives an integer or a float literal its type. The result is a cast
      of the literal, and an integer has to fit the type
    - it turns a write reference into a read reference to the same type in
      the same region. The result is a cast too

    Any other expression has to have the annotated type, and is returned as
    it is. If it does not have it, the error says that the cast is not
    valid. *)
val check_annotated : expr_ctx -> aexpr -> ty -> texpr

val cast_arguments : type_bindings -> value_parameter list -> texpr list -> texpr list
