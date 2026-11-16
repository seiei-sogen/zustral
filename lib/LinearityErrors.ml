(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The linearity errors, and their wording. *)
open Identifier
open Common
open Type
open ErrorText

(** The state of a variable, as an error names it. The checker's own type of
    states is [LinearityCheck.var_state]. This module is below the checker
    and cannot use that type, so the checker converts where it raises. *)
type reported_state =
  | ReportedUnconsumed
  | ReportedBorrowedRead
  | ReportedBorrowedWrite
  | ReportedConsumed

let humanize_state (state: reported_state): string =
  match state with
  | ReportedUnconsumed -> "not yet consumed"
  | ReportedBorrowedRead -> "borrowed (read-only)"
  | ReportedBorrowedWrite -> "borrowed (read-write)"
  | ReportedConsumed -> "consumed"

(** The statements whose branches must leave every variable in the same state. *)
type branching =
  | CaseBranching
  | IfBranching

let branching_words (statement: branching): string =
  match statement with
  | CaseBranching -> "a case"
  | IfBranching -> "an if"

(** The expressions that evaluate only some of their operands. *)
type partly_evaluated_expression =
  | IfExpressionBranches
  | ConjunctionRightOperand
  | DisjunctionRightOperand

let where_consumed_on_some_paths_only (expression: partly_evaluated_expression): string =
  match expression with
  | IfExpressionBranches -> "in one branch of an if expression but not in the other"
  | ConjunctionRightOperand -> "in the right operand of a conjunction (and), which is evaluated only when the left operand is true"
  | DisjunctionRightOperand -> "in the right operand of a disjunction (or), which is evaluated only when the left operand is false"

type t =
  | DereferenceNonFree of ty
  | FreeContainsLinear of { name: identifier; universe: universe; parameter: bool }
  | ForgotToConsume of identifier
  | ConsumedInLoop of identifier list
  | AssignedInLoop of identifier list
  | InconsistentBranches of { name: identifier; statement: branching; state_a: reported_state; state_b: reported_state }
  | ConsumedMoreThanOnce of identifier
  | ConsumedOnSomePathsOnly of { name: identifier; expression: partly_evaluated_expression }
  | ConsumedWhileBorrowedRead of identifier
  | ConsumedWhileBorrowedWrite of identifier
  | AlreadyConsumed of identifier
  | AssignToUnconsumed of identifier
  | AssignToBorrowed of { name: identifier; state: reported_state }
  | CannotBorrow of { mode: borrow_stmt_kind; name: identifier; state: reported_state }
  | NotConsumedAtReturn of identifier
  | AssignToNonFreePath of ty

(* The start of a sentence about one variable or several. *)
let the_variables_were (names: identifier list): err_text =
  let c: int = List.length names in
  [
    Text "The ";
    if (c = 1) then
      Text "variable "
    else
      Text "variables ";
    Code (String.concat ", " (List.map ident_string names));
    if (c = 1) then
      Text " was "
    else
      Text " were "
  ]

let render (error: t): err_text =
  match error with
  | DereferenceNonFree ty ->
     [
       Text "Cannot dereference non-free type ";
       Type ty
     ]
  | FreeContainsLinear { name; universe; parameter } ->
     [
       Code (ident_string name);
       Text " was declared to belong to the ";
       Code "Free";
       Text " universe, but it ";
       Text (if parameter then "has a type parameter" else "contains a type");
       Text " that belongs to the ";
       Code (universe_string universe);
       Text " universe."
     ]
  | ForgotToConsume name ->
     [
       Text "Forgot to consume a linear variable: ";
       Code (ident_string name);
       Text "."
     ]
  | ConsumedInLoop names ->
     the_variables_were names
     @ [ Text "consumed in the loop, without afterwards being reassigned." ]
  | AssignedInLoop names ->
     the_variables_were names
     @ [ Text "consumed before the loop and assigned in the loop, without afterwards being consumed again." ]
  | InconsistentBranches { name; statement; state_a; state_b } ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text " is used inconsistently in the branches of ";
       Text (branching_words statement);
       Text " statement. In one branch it is ";
       Text (humanize_state state_a);
       Text " while in the other it is ";
       Text (humanize_state state_b);
       Text "."
     ]
  | ConsumedMoreThanOnce name ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text " is consumed multiple times within the same expression.";
     ]
  | ConsumedOnSomePathsOnly { name; expression } ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text " is consumed ";
       Text (where_consumed_on_some_paths_only expression);
       Text "."
     ]
  | ConsumedWhileBorrowedRead name ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text " cannot be consumed while it is borrowed (immutably).";
     ]
  | ConsumedWhileBorrowedWrite name ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text "cannot be consumed while it is borrowed (mutably).";
     ]
  | AlreadyConsumed name ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text " has already been consumed.";
     ]
  | AssignToUnconsumed name ->
     [
       Text "Cannot assign to the variable ";
       Code (ident_string name);
       Text " because it is not yet consumed."
     ]
  | AssignToBorrowed { name; state } ->
     [
       Text "Cannot assign to the variable ";
       Code (ident_string name);
       Text " because it is ";
       Text (humanize_state state);
       Text "."
     ]
  | CannotBorrow { mode; name; state } ->
     let mode_string = match mode with
       | Read -> "borrow (immutably)"
       | Write -> "borrow (mutably)"
       | Reborrow -> "reborrow"
     in
     [
       Text ("Cannot " ^ mode_string ^ " the variable ");
       Code (ident_string name);
       Text " because it is already ";
       Text (humanize_state state);
       Text "."
     ]
  | NotConsumedAtReturn name ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text " is not consumed by the time of the return statement. Did you forget to call a destructor, or destructure the contents?"
     ]
  | AssignToNonFreePath ty ->
     [
       Text "Cannot assign to a path that ends in a type, ";
       Type ty;
       Text ", not in the Free universe."
     ]
