(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)

(** Desugar paths into reference transforms. *)
open Stages

val transform_expr : AstLC.aexpr -> AstDP.aexpr

val transform_stmt : AstLC.astmt -> AstDP.astmt
