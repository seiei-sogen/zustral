(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** Utils for the TAST *)
open Type
open Stages.Tast
open Identifier

(* This function converts an argument list to a list of typed expressions.

   The second argument is the list of parameter names of the function being
   called.

   If the argument list is a positional argument list, we return list is
   returned unchanged.

   If this is a named argument list, we convert it to a positional list using
   the given list of parameter names.

   For example, if the given list of parameter names is ["a", "b", "c"], and the
   list is a named list with a map like {"b" => 3, "a" => 3.14, "c" =>
   "hello!"}, this returns the list [3.14, 3, "hello!"].

   Checks:

       1. The list of parameter names has the same size as the argument
          list.
       2. If the argument list is a named argument list, then the set of keys
          is exactly the same as the set of given parameter names.
*)
val arglist_to_positional : Error.place -> typed_arglist * identifier list -> texpr list

(** The order a call wrote its arguments in, from the argument list and the
    names of the parameters. Call it after {!arglist_to_positional}, which
    checks that a named list names each parameter once. *)
val written_order_of_arguments : typed_arglist * identifier list -> written_argument_order

val get_type : Error.place -> texpr -> ty

(** Whether an expression may be the value of a constant: literals, other
    constants, functions, constructors and casts of those, and the accesses
    that do not read memory. A call is not, and neither is anything that names
    a parameter, a local variable or a temporary. *)
val is_constant_expression : texpr -> bool
