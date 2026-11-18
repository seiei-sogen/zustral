(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Type
open Span
open Stages.Tast
open EvaluationOrderSteps
open EvaluationOrderDependence

(* No other name is this one: a name of the source starts with a letter, and a
   temporary of LiftControlPass is `_t` and a number. The index counts the
   temporaries of one function. *)
let temporary_name (index: int): identifier =
  make_ident ("_evaluated_in_order_" ^ string_of_int index)

(* The functions below take the same first two arguments and return the same
   kind of result as `fix_evaluation_order_of_expression`: the interface says
   what they are. *)
let rec fix_evaluation_order_of_expression (write_borrowed_variables: identifier list) (next_temporary: int) (expr: texpr): step list * texpr * int =
  match expr with
  | TNilConstant | TBoolConstant _ | TIntConstant _ | TFloatConstant _ | TStringConstant _
  | TConstVar _ | TParamVar _ | TLocalVar _ | TFunVar _ | TTemporary _ | TSizeOf _ ->
     ([], expr, next_temporary)
  | TFuncall (id, name, arguments, rt, substs, written_order) ->
     let (steps, arguments, next_temporary) = fix_evaluation_order_of_call_arguments write_borrowed_variables next_temporary written_order arguments in
     (steps, TFuncall (id, name, arguments, rt, substs, written_order), next_temporary)
  | TMethodCall (id, name, typarams, arguments, rt, substs, written_order) ->
     let (steps, arguments, next_temporary) = fix_evaluation_order_of_call_arguments write_borrowed_variables next_temporary written_order arguments in
     (steps, TMethodCall (id, name, typarams, arguments, rt, substs, written_order), next_temporary)
  | TVarMethodCall call ->
     let (steps, args, next_temporary) = fix_evaluation_order_of_call_arguments write_borrowed_variables next_temporary call.written_order call.args in
     (steps, TVarMethodCall { call with args }, next_temporary)
  | TFptrCall (pointer, parameter_types, arguments, rt) ->
     let (steps, arguments, next_temporary) = fix_evaluation_order_of_operands write_borrowed_variables next_temporary arguments in
     (steps, TFptrCall (pointer, parameter_types, arguments, rt), next_temporary)
  | TEmbed (ty, c_text, arguments) ->
     let (steps, arguments, next_temporary) = fix_evaluation_order_of_operands write_borrowed_variables next_temporary arguments in
     (steps, TEmbed (ty, c_text, arguments), next_temporary)
  | TRecordConstructor (ty, values) ->
     let (steps, values, next_temporary) = fix_evaluation_order_of_constructor_values write_borrowed_variables next_temporary values in
     (steps, TRecordConstructor (ty, values), next_temporary)
  | TUnionConstructor constructor ->
     let (steps, args, next_temporary) = fix_evaluation_order_of_constructor_values write_borrowed_variables next_temporary constructor.args in
     (steps, TUnionConstructor { constructor with args }, next_temporary)
  | TComparison (operator, left, right) ->
     let (steps, left, right, next_temporary) = fix_evaluation_order_of_two_operands write_borrowed_variables next_temporary left right in
     (steps, TComparison (operator, left, right), next_temporary)
  | TArrayIndex (indexed, index, element_type, region, mode) ->
     let (steps, indexed, index, next_temporary) = fix_evaluation_order_of_two_operands write_borrowed_variables next_temporary indexed index in
     (steps, TArrayIndex (indexed, index, element_type, region, mode), next_temporary)
  | TSpanIndex (indexed, index, element_type, region, mode) ->
     let (steps, indexed, index, next_temporary) = fix_evaluation_order_of_two_operands write_borrowed_variables next_temporary indexed index in
     (steps, TSpanIndex (indexed, index, element_type, region, mode), next_temporary)
  | TCast (operand, ty) ->
     let (steps, operand, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary operand in
     (steps, TCast (operand, ty), next_temporary)
  | TImplicitCast (operand, ty) ->
     let (steps, operand, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary operand in
     (steps, TImplicitCast (operand, ty), next_temporary)
  | TNegation operand ->
     let (steps, operand, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary operand in
     (steps, TNegation operand, next_temporary)
  | TArithmeticNegation operand ->
     let (steps, operand, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary operand in
     (steps, TArithmeticNegation operand, next_temporary)
  | TDeref (operand, ty) ->
     let (steps, operand, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary operand in
     (steps, TDeref (operand, ty), next_temporary)
  | TSlotAccessor (operand, slot, ty) ->
     let (steps, operand, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary operand in
     (steps, TSlotAccessor (operand, slot, ty), next_temporary)
  | TPointerSlotAccessor (operand, slot, ty) ->
     let (steps, operand, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary operand in
     (steps, TPointerSlotAccessor (operand, slot, ty), next_temporary)
  | TConjunction (left, right) ->
     evaluate_right_operand_on_one_path write_borrowed_variables next_temporary left right
       (fun left right -> TConjunction (left, right))
       (fun left_is_true evaluate_right -> TIf (empty_span, left_is_true, evaluate_right, TSkip empty_span))
  | TDisjunction (left, right) ->
     evaluate_right_operand_on_one_path write_borrowed_variables next_temporary left right
       (fun left right -> TDisjunction (left, right))
       (fun left_is_true evaluate_right -> TIf (empty_span, left_is_true, TSkip empty_span, evaluate_right))
  | TIfExpression (condition, if_true, if_false) ->
     let (steps_of_condition, condition, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary condition in
     let (steps_if_true, if_true, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary if_true in
     let (steps_if_false, if_false, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary if_false in
     (match (steps_if_true, steps_if_false) with
      | ([], []) ->
         (steps_of_condition, TIfExpression (condition, if_true, if_false), next_temporary)
      | (_ :: _, _) | ([], _ :: _) ->
         (* As for `and` and `or` below: the steps of a branch run only when
            the branch is chosen. *)
         let ty: ty = TastUtil.get_type Error.NoSpan if_true in
         let result: identifier = temporary_name next_temporary in
         let evaluate_branch (steps: step list) (value: texpr): tstmt =
           statement_after_steps steps (TAssignTmp (result, value))
         in
         (steps_of_condition
          @ [DeclareTemporary (result, ty);
             EvaluateOnOnePath (TIf (empty_span, condition, evaluate_branch steps_if_true if_true, evaluate_branch steps_if_false if_false))],
          TTemporary (result, ty),
          next_temporary + 1))

(* `and` and `or`. C evaluates the right operand of && and of || on one path
   only, and the steps of the right operand must run on that path only. When
   it has steps, an `if` statement takes the place of the operator. *)
and evaluate_right_operand_on_one_path
      (write_borrowed_variables: identifier list)
      (next_temporary: int)
      (left: texpr)
      (right: texpr)
      (operation: texpr -> texpr -> texpr)
      (if_statement: texpr -> tstmt -> tstmt): step list * texpr * int =
  let (steps_of_left, left, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary left in
  let (steps_of_right, right, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary right in
  match steps_of_right with
  | [] ->
     (steps_of_left, operation left right, next_temporary)
  | _ :: _ ->
     let result: identifier = temporary_name next_temporary in
     let value_of_left_then_of_whole: texpr = TTemporary (result, Boolean) in
     let evaluate_right: tstmt = statement_after_steps steps_of_right (TAssignTmp (result, right)) in
     (steps_of_left
      @ [EvaluateIntoNewTemporary (result, Boolean, left);
         EvaluateOnOnePath (if_statement value_of_left_then_of_whole evaluate_right)],
      value_of_left_then_of_whole,
      next_temporary + 1)

(* A call holds its arguments in the order of the parameters. *)
and fix_evaluation_order_of_call_arguments
      (write_borrowed_variables: identifier list)
      (next_temporary: int)
      (written_order: written_argument_order)
      (arguments: texpr list): step list * texpr list * int =
  let as_written: texpr list = List.map (List.nth arguments) written_order in
  let (steps, as_written, next_temporary) = fix_evaluation_order_of_operands write_borrowed_variables next_temporary as_written in
  let argument_of_parameter: (int * texpr) list = List.combine written_order as_written in
  (steps, List.mapi (fun parameter _ -> List.assoc parameter argument_of_parameter) arguments, next_temporary)

and fix_evaluation_order_of_constructor_values (write_borrowed_variables: identifier list) (next_temporary: int) (values: (identifier * texpr) list): step list * (identifier * texpr) list * int =
  let (names, as_written): identifier list * texpr list = List.split values in
  let (steps, as_written, next_temporary) = fix_evaluation_order_of_operands write_borrowed_variables next_temporary as_written in
  (steps, List.combine names as_written, next_temporary)

and fix_evaluation_order_of_two_operands (write_borrowed_variables: identifier list) (next_temporary: int) (first: texpr) (second: texpr): step list * texpr * texpr * int =
  let (steps_of_first, first, next_temporary) = evaluate_operand_before_later_operands write_borrowed_variables next_temporary first [second] in
  let (steps_of_second, second, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary second in
  (steps_of_first @ steps_of_second, first, second, next_temporary)

and fix_evaluation_order_of_operands (write_borrowed_variables: identifier list) (next_temporary: int) (operands: texpr list): step list * texpr list * int =
  match operands with
  | [] ->
     ([], [], next_temporary)
  | first :: later ->
     let (steps_of_first, first, next_temporary) = evaluate_operand_before_later_operands write_borrowed_variables next_temporary first later in
     let (steps_of_later, later, next_temporary) = fix_evaluation_order_of_operands write_borrowed_variables next_temporary later in
     (steps_of_first @ steps_of_later, first :: later, next_temporary)

(* An operand of an operation, and the operands the operation evaluates after
   it. C does not say which of two operands it evaluates first. When that can
   show, a step evaluates the earlier one into a temporary. *)
and evaluate_operand_before_later_operands (write_borrowed_variables: identifier list) (next_temporary: int) (operand: texpr) (later_operands: texpr list): step list * texpr * int =
  let (steps, operand, next_temporary) = fix_evaluation_order_of_expression write_borrowed_variables next_temporary operand in
  if depends_on_evaluation_order write_borrowed_variables operand
     && List.exists (depends_on_evaluation_order write_borrowed_variables) later_operands then
    let ty: ty = TastUtil.get_type Error.NoSpan operand in
    let evaluated_earlier: identifier = temporary_name next_temporary in
    (steps @ [EvaluateIntoNewTemporary (evaluated_earlier, ty, operand)], TTemporary (evaluated_earlier, ty), next_temporary + 1)
  else
    (steps, operand, next_temporary)
