(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Stages.Tast

(** Whether it can show when the expression is evaluated, among the operands of
    one operation (issue #53). It can for two kinds of expression.

    One does something a program can see: a call, an [@embed], or an operation
    that can stop the program (an index, the negation of the least value of a
    signed integer type). The arithmetic operators are calls.

    The other reads a variable that a write borrow covers: an operand evaluated
    in between can change the variable through the reference. The first
    argument is the list of those variables.

    Nothing else that a safe module reads can change while an expression is
    evaluated. A read borrow keeps its variable from being written. The
    linearity check rejects two uses of one write reference in one expression.
    A constant is taken to be fixed, although it is a C macro and its value can
    hold an [@embed]. *)
val depends_on_evaluation_order : identifier list -> texpr -> bool
