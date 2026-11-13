(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Stages

(** Rewrite the statements and expressions of a module into the smaller
    language the later stages take. It fails on a borrow expression in a place
    that allows none, and on an assignment to something that is not a path. *)
val desugar : Combined.combined_module -> SmallCombined.combined_module Outcome.t