(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Stages.Tast

(** The body of a function or of a method, with every expression in a form
    that evaluates its operands in the order the program wrote them
    ({!EvaluationOrderOfExpression.fix_evaluation_order_of_expression}). The
    steps of an expression run just before the statement that holds it. *)
val fix_evaluation_order_of_statement : tstmt -> tstmt
