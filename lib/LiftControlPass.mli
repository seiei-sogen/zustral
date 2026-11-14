(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Stages

(** Lift complex expressions in control structures. The names of the
    temporaries that hold them come from the supply. *)
val lift: Supply.t -> Ast.astmt -> AstLC.astmt * Supply.t

val transform: Ast.aexpr -> AstLC.aexpr