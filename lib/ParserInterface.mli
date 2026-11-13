(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Cst

(** Parse the text of a module interface. The second argument is the name of
    the file, for the positions in errors. *)
val parse_module_int : string -> string -> concrete_module_interface Outcome.t

(** Parse the text of a module body. *)
val parse_module_body : string -> string -> concrete_module_body Outcome.t

(** Parse an expression. Raises [Austral_error] if it does not parse. *)
val parse_expr : string -> cexpr
