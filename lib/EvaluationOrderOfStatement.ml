(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Common
open Stages.Tast
open EvaluationOrderSteps
open EvaluationOrderOfExpression

let fix_evaluation_order_of_statement (body: tstmt): tstmt =
  (* The arguments are the variables that a write borrow around the statement
     covers, and the number of temporaries made so far in the body, which
     names the next one. *)
  let rec fix_evaluation_order_of_nested_statement (write_borrowed_variables: identifier list) (next_temporary: int) (stmt: tstmt): tstmt * int =
    let statement_of_one_expression (expr: texpr) (statement: texpr -> tstmt): tstmt * int =
      let (steps, expr, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary expr in
      (statement_after_steps steps (statement expr), next_temporary)
    in
    match stmt with
    | TSkip _ ->
       (stmt, next_temporary)
    | TAssignVar (span, name, value) ->
       statement_of_one_expression value (fun value -> TAssignVar (span, name, value))
    | TInitialAssign (name, value) ->
       statement_of_one_expression value (fun value -> TInitialAssign (name, value))
    | TDiscarding (span, value) ->
       statement_of_one_expression value (fun value -> TDiscarding (span, value))
    | TReturn (span, value) ->
       statement_of_one_expression value (fun value -> TReturn (span, value))
    | TLetTmp (name, ty, value) ->
       statement_of_one_expression value (fun value -> TLetTmp (name, ty, value))
    | TAssignTmp (name, value) ->
       statement_of_one_expression value (fun value -> TAssignTmp (name, value))
    | TAssign (span, lvalue, value) ->
       (* The value is a temporary that the statement before computed
          (LiftControlPass), so only the path to the assigned place has
          operands. *)
       statement_of_one_expression lvalue (fun lvalue -> TAssign (span, lvalue, value))
    | TDestructure (span, mutability, bindings, value, body) ->
       let (steps, value, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary value in
       let (body, next_temporary) = fix_evaluation_order_of_nested_statement write_borrowed_variables next_temporary body in
       (statement_after_steps steps (TDestructure (span, mutability, bindings, value, body)), next_temporary)
    | TLet (span, mutability, name, ty, body) ->
       let (body, next_temporary) = fix_evaluation_order_of_nested_statement write_borrowed_variables next_temporary body in
       (TLet (span, mutability, name, ty, body), next_temporary)
    | TBlock (span, first, second) ->
       let (first, next_temporary) = fix_evaluation_order_of_nested_statement write_borrowed_variables next_temporary first in
       let (second, next_temporary) = fix_evaluation_order_of_nested_statement write_borrowed_variables next_temporary second in
       (TBlock (span, first, second), next_temporary)
    | TBorrow borrow ->
       let write_borrowed_in_body: identifier list =
         match borrow.mode with
         | Read -> write_borrowed_variables
         | Write | Reborrow -> borrow.original :: write_borrowed_variables
       in
       let (body, next_temporary) = fix_evaluation_order_of_nested_statement write_borrowed_in_body next_temporary borrow.body in
       (TBorrow { borrow with body }, next_temporary)
    (* The condition of an `if` and of a `while`, the value of a `case` and
       the bounds of a `for` are temporaries that the statements before
       computed (LiftControlPass): they have no operands. *)
    | TIf (span, condition, if_true, if_false) ->
       let (if_true, next_temporary) = fix_evaluation_order_of_nested_statement write_borrowed_variables next_temporary if_true in
       let (if_false, next_temporary) = fix_evaluation_order_of_nested_statement write_borrowed_variables next_temporary if_false in
       (TIf (span, condition, if_true, if_false), next_temporary)
    | TWhile (span, condition, body) ->
       let (body, next_temporary) = fix_evaluation_order_of_nested_statement write_borrowed_variables next_temporary body in
       (TWhile (span, condition, body), next_temporary)
    | TFor (span, name, initial, final, body) ->
       let (body, next_temporary) = fix_evaluation_order_of_nested_statement write_borrowed_variables next_temporary body in
       (TFor (span, name, initial, final, body), next_temporary)
    | TCase (span, value, union_id, type_arguments, whens, case_ref) ->
       let (next_temporary, whens) =
         Util.map_with_context
           (fun (next_temporary, TypedWhen (case_name, bindings, body)) ->
             let (body, next_temporary) = fix_evaluation_order_of_nested_statement write_borrowed_variables next_temporary body in
             (next_temporary, TypedWhen (case_name, bindings, body)))
           next_temporary
           whens
       in
       (TCase (span, value, union_id, type_arguments, whens, case_ref), next_temporary)
  in
  fst (fix_evaluation_order_of_nested_statement [] 0 body)
