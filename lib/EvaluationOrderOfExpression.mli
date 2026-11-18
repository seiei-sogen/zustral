(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Stages.Tast
open EvaluationOrderSteps

(** The expression in a form that evaluates the operands of every operation in
    the order the program wrote them, whatever order the C compiler evaluates
    the operands of a C operation in (issue #53). Returns the steps that run
    before the statement the expression is in, and the expression that the
    statement then evaluates. Running the steps and evaluating that expression
    does what evaluating the given expression from left to right does.

    The operands of an operation are the arguments of a call, of a constructor
    and of an [@embed] in the order the program wrote them, the two sides of
    an arithmetic operator and of a comparison, and an indexed value and its
    index. An operand moves to a step, which evaluates it into a temporary,
    when it and an operand after it depend on the order
    ({!EvaluationOrderDependence.depends_on_evaluation_order}). Every other
    operand stays where it is.

    [and], [or] and [if] evaluate a part of their operands on one path only. A
    step of such a part goes in the branch of an [if] statement, and the
    statement assigns the value of the whole expression to a temporary.

    The arguments are the variables that a write borrow covers where the
    expression is, the number of temporaries this stage has made in the
    function so far, and the expression. The number comes back with the
    temporaries of the steps added. *)
val fix_evaluation_order_of_expression : identifier list -> int -> texpr -> step list * texpr * int
