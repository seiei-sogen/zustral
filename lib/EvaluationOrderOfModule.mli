(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Stages.Tast

(** The stage that fixes the order of evaluation (issue #53).

    Zustral evaluates the operands of an operation in the order the program
    writes them. The C that the compiler generates has the operands of an
    operation as the operands of one C operation, and C does not say in which
    order those are evaluated. This stage rewrites the bodies of the functions
    and the methods so that the order of C cannot show
    ({!EvaluationOrderOfStatement.fix_evaluation_order_of_statement}).

    It runs after the linearity check, so the type checker and the linearity
    check see the expressions as the program wrote them, and before the bodies
    go into the environment, so every monomorph of a generic function is made
    from the rewritten body. It cannot fail.

    The value of a constant is not rewritten: it becomes a C macro, which has
    no statement to run a step before. *)

(** A module that passed the linearity check and whose bodies evaluate their
    operands in the order the program wrote them. Only
    {!fix_evaluation_order_of_module} makes one, so a stage that takes this
    type cannot be given a module that skipped this stage. *)
type ordered_module

val fix_evaluation_order_of_module : LinearityCheck.checked_module -> ordered_module

(** The module with the rewritten bodies. *)
val as_typed_module : ordered_module -> typed_module
