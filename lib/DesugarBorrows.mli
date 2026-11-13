(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Stages

(** Desugar anonymous borrows. The names of the references and of their
    regions come from the supply. *)

(** A borrow in the expression is an error. *)
val transform_expr : Supply.t -> AstDP.aexpr -> AstDB.aexpr

val transform_stmt : Supply.t -> AstDP.astmt -> AstDB.astmt * Supply.t
