(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Type
open Stages.Tast

(** What runs before a statement so that an expression of the statement finds
    its earlier operands evaluated, in the order the program wrote them (issue
    #53). *)
type step =
  | EvaluateIntoNewTemporary of identifier * ty * texpr
  (** Declares the temporary with the value of the expression. *)
  | DeclareTemporary of identifier * ty
  (** Declares the temporary with no value. A later step gives it one. *)
  | EvaluateOnOnePath of tstmt
  (** An [if] statement whose branches evaluate the operands that only one
      path evaluates, and assign a temporary declared before it. *)

(** The statement that runs the steps, from the first to the last, and then the
    given statement. *)
val statement_after_steps : step list -> tstmt -> tstmt
