(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Stages.Tast
open Type
open TypeSystem
open Error

(* Data structures *)

type var_state =
  | Unconsumed
  | BorrowedRead
  | BorrowedWrite
  | Consumed
[@@deriving show]

(* The error module is below this one and has its own names for the states
   an error mentions. *)
let reported_state (state: var_state): LinearityErrors.reported_state =
  match state with
  | Unconsumed -> ReportedUnconsumed
  | BorrowedRead -> ReportedBorrowedRead
  | BorrowedWrite -> ReportedBorrowedWrite
  | Consumed -> ReportedConsumed

let is_consumed (state: var_state): bool =
  match state with
  | Consumed -> true
  | Unconsumed | BorrowedRead | BorrowedWrite -> false

let universe_linear_ish = function
  | LinearUniverse -> true
  | TypeUniverse -> true
  | FreeUniverse | RegionUniverse -> false

type state_tbl = StateTable of (identifier * ty * var_state) list

let show_state_tbl (tbl: state_tbl): string =
  let (StateTable entries) = tbl in
  "Entries:\n"
  ^ (String.concat "\n" (List.map (fun (n, _, v) -> (ident_string n) ^ "\t" ^ (show_var_state v)) entries))

let empty_tbl: state_tbl = StateTable []

let table_rows (tbl: state_tbl): (identifier * ty * var_state) list =
  let (StateTable rows) = tbl in
  rows

let get_entry (tbl: state_tbl) (name: identifier): (ty * var_state) option =
  match (List.find_opt (fun (n, _, _) -> equal_identifier name n) (table_rows tbl)) with
  | Some (_, ty, state) ->
     Some (ty, state)
  | None ->
     None

let get_entry_or_fail (place: place) (tbl: state_tbl) (name: identifier): (ty * var_state) =
  match get_entry tbl name with
  | Some p -> p
  | None ->
     (* Depends on the stage: the type checker has rejected a use of an
        undeclared variable, and this pass adds each variable to the table where
        it is declared. *)
     internal_err place ("variable `"
          ^ (ident_string name)
          ^ "` not in state table. Table contents: \n\n"
          ^ (show_state_tbl tbl))

let add_entry (place: place) (tbl: state_tbl) (name: identifier) (ty: ty): state_tbl =
  let (StateTable rows) = tbl in
  match get_entry tbl name with
  | None ->
     StateTable ((name, ty, Unconsumed) :: rows)
  | Some _ ->
     (* The justification for this being an internal error is that the compiler
        should already have caught a duplicate variable. *)
     internal_err place ("An entry exists in the state table with this name: " ^ (ident_string name))

(* A write reference and a write span are in the Linear universe, so that they
   are not copied, and can be dropped without being used. *)
let need_not_be_consumed (ty: ty): bool =
  match ty with
  | WriteRef _ | SpanMut _ ->
     true
  | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
  | ReadRef _ | Span _ | TyVar _ | Address _ | Pointer _ | FnPtr _ ->
     false

let trim_free (tbl: state_tbl): state_tbl =
  let (StateTable entries) = tbl in
  let trim tuple =
    let (_, ty, _) = tuple in
    if universe_linear_ish (type_universe ty) then
      Some tuple
    else
      None
  in
  StateTable (List.filter_map trim entries)

(* A name this pass updates or removes is in the table: the statement that
   declares it added it, and every statement removes only the names it added
   itself, which `add_entry` keeps distinct. *)
let update_tbl (place: place) (tbl: state_tbl) (name: identifier) (state: var_state): state_tbl =
  let (ty, _) = get_entry_or_fail place tbl name in
  let (StateTable rows) = tbl in
  let other_entries = List.filter (fun (n, _, _) -> not (equal_identifier name n)) rows
  in
  StateTable ((name, ty, state) :: other_entries)

let remove_entry (place: place) (tbl: state_tbl) (name: identifier): state_tbl =
  match get_entry_or_fail place tbl name with
  | (ty, state) ->
     if is_consumed state || need_not_be_consumed ty then
       let (StateTable rows) = tbl in
       let others = List.filter (fun (n, _, _) -> not (equal_identifier name n)) rows in
       StateTable others
     else
       if universe_linear_ish (type_universe ty) then
         linearity_error place (ForgotToConsume name)
       else
         let (StateTable rows) = tbl in
         let others = List.filter (fun (n, _, _) -> not (equal_identifier name n)) rows in
         StateTable others

let rec remove_entries (place: place) (tbl: state_tbl) (names: identifier list): state_tbl =
  match names with
  | first::rest ->
     remove_entries place (remove_entry place tbl first) rest
  | [] ->
     tbl

let tbl_to_list (tbl: state_tbl): (identifier * ty * var_state) list =
  table_rows tbl

(* How many times the evaluation of an expression consumes a variable. *)
type appearances =
  | OnEveryPath of int
  (* The same number of times, whichever branches the evaluation takes. *)
  | OnSomePathsOnly of { at_most: int; parts_at: LinearityErrors.partly_evaluated_expression }
  (* Fewer times on one path than on another, and never more than `at_most`
     times. The paths part at the expression `parts_at`. *)

let zero_appearances: appearances = OnEveryPath 0

let consumed_once: appearances = OnEveryPath 1

let most_times (appearances: appearances): int =
  match appearances with
  | OnEveryPath times -> times
  | OnSomePathsOnly { at_most; _ } -> at_most

(* The appearances in two expressions that are evaluated one after the other. *)
let merge (a: appearances) (b: appearances): appearances =
  match (a, b) with
  | (OnEveryPath times_in_a, OnEveryPath times_in_b) ->
     OnEveryPath (times_in_a + times_in_b)
  | (OnSomePathsOnly { parts_at; _ }, (OnEveryPath _ | OnSomePathsOnly _))
  | (OnEveryPath _, OnSomePathsOnly { parts_at; _ }) ->
     OnSomePathsOnly { at_most = most_times a + most_times b; parts_at }

(* The appearances in the two branches of an `if` expression. The evaluation
   takes one of the branches. *)
let merge_if_expression_branches (then_branch: appearances) (else_branch: appearances): appearances =
  let at_most: int = max (most_times then_branch) (most_times else_branch) in
  match (then_branch, else_branch) with
  | (OnEveryPath times_in_then, OnEveryPath times_in_else) ->
     if times_in_then = times_in_else then
       then_branch
     else
       OnSomePathsOnly { at_most; parts_at = IfExpressionBranches }
  | (OnSomePathsOnly { parts_at; _ }, (OnEveryPath _ | OnSomePathsOnly _))
  | (OnEveryPath _, OnSomePathsOnly { parts_at; _ }) ->
     OnSomePathsOnly { at_most; parts_at }

(* The appearances in the right operand of `and` or of `or`. The evaluation
   skips that operand when the left one decides the result. *)
let in_right_operand_evaluated_on_some_paths (expression: LinearityErrors.partly_evaluated_expression) (right_operand: appearances): appearances =
  match right_operand with
  | OnEveryPath times ->
     if times = 0 then
       right_operand
     else
       OnSomePathsOnly { at_most = times; parts_at = expression }
  | OnSomePathsOnly _ ->
     right_operand

let merge_list (l: appearances list): appearances =
  List.fold_left merge zero_appearances l

(* Counting appearances of variables in expressions *)

let rec count (name: identifier) (expr: texpr): appearances =
  let c = count name in
  match expr with
  | TNilConstant ->
     zero_appearances
  | TBoolConstant _ ->
     zero_appearances
  | TIntConstant _ ->
     zero_appearances
  | TFloatConstant _ ->
     zero_appearances
  | TStringConstant _ ->
     zero_appearances
  | TConstVar _ ->
     (* Constants variables can't be linear. *)
     zero_appearances
  | TParamVar (name', _) ->
     if equal_identifier name name' then
       consumed_once
     else
       zero_appearances
  | TLocalVar (name', _) ->
     if equal_identifier name name' then
       consumed_once
     else
       zero_appearances
  | TTemporary (name', _) ->
     if equal_identifier name name' then
       consumed_once
     else
       zero_appearances
  | TFunVar _ ->
     zero_appearances
  | TFuncall (_, _, args, _, _) ->
     merge_list (List.map c args)
  | TMethodCall (_, _, _, args, _, _) ->
     merge_list (List.map c args)
  | TVarMethodCall { args; _ } ->
     merge_list (List.map c args)
  | TFptrCall (_, _, args, _) ->
     merge_list (List.map c args)
  | TCast (e, _) | TImplicitCast (e, _) ->
     c e
  | TComparison (_, lhs, rhs) ->
     merge (c lhs) (c rhs)
  | TConjunction (lhs, rhs) ->
     merge (c lhs) (in_right_operand_evaluated_on_some_paths ConjunctionRightOperand (c rhs))
  | TDisjunction (lhs, rhs) ->
     merge (c lhs) (in_right_operand_evaluated_on_some_paths DisjunctionRightOperand (c rhs))
  | TNegation e ->
     c e
  | TArithmeticNegation e ->
     c e
  | TIfExpression (e, t, f) ->
     merge (c e) (merge_if_expression_branches (c t) (c f))
  | TRecordConstructor (_, args) ->
     merge_list (List.map (fun (_, e) -> c e) args)
  | TUnionConstructor { args; _ } ->
     merge_list (List.map (fun (_, e) -> c e) args)
  | TEmbed (_, _, args) ->
     merge_list (List.map c args)
  | TDeref (e, _) ->
     c e
  | TSizeOf _ ->
     zero_appearances
  | TSlotAccessor (e, _, _) ->
     c e
  | TPointerSlotAccessor (e, _, _) ->
     c e
  | TArrayIndex (e, i, _, _, _) ->
     merge (c e) (c i)
  | TSpanIndex (e, i, _, _, _) ->
     merge (c e) (c i)
 
(* Utilities *)

let get_state (place: place) (tbl: state_tbl) (name: identifier): var_state =
  let (_, state) = get_entry_or_fail place tbl name in
  state

type partitions =
  | Zero
  | One
  | MoreThanOne

(* The argument counts appearances: a sum of zeros and ones, so never
   negative. Everything below one is Zero. *)
let partition (n: int): partitions =
  if n > 1 then
    MoreThanOne
  else
    if n = 1 then
      One
    else
      Zero

(* Table consistency *)

let tables_are_consistent (place: place) (statement: LinearityErrors.branching) (a: state_tbl) (b: state_tbl): unit =
  let a: state_tbl = trim_free a
  and b: state_tbl = trim_free b in
  (* The variables both tables have, each with its state in A and its state
     in B. *)
  let common: (identifier * var_state * var_state) list =
    List.filter_map (fun (name, _, state_a) ->
        match get_entry b name with
        | Some (_, state_b) -> Some (name, state_a, state_b)
        | None -> None)
      (tbl_to_list a)
  in
  (* Ensure the states are the same. *)
  List.iter (fun (name, state_a, state_b) ->
      if state_a <> state_b then
        linearity_error place (InconsistentBranches { name; statement; state_a = reported_state state_a; state_b = reported_state state_b })
      else
        ())
    common

(* The branches of a `case` leave every variable in the same state when each
   of them agrees with the first. *)
let case_branch_tables_are_consistent (place: place) (lst: state_tbl list): unit =
  match lst with
  | first :: rest ->
     List.iter (tables_are_consistent place CaseBranching first) rest
  | [] ->
     ()

(* Linearity checking in expressions *)

let rec check_var_in_expr (place: place) (tbl: state_tbl) (name: identifier) (ty: ty) (expr: texpr): state_tbl =
  (* Count the appearances of the variable in the expression. *)
  match count name expr with
  | OnSomePathsOnly { at_most; parts_at } ->
     if at_most > 1 || need_not_be_consumed ty then
       (* Checked as if every path consumed the variable `at_most` times. More
          than once is an error whichever path the evaluation takes. A value
          that need not be consumed loses nothing on the path that does not
          use it, and cannot be used again after a path that may have. *)
       check_var_consumed_on_every_path place tbl name ty at_most
     else if universe_linear_ish (type_universe ty) then
       linearity_error place (ConsumedOnSomePathsOnly { name; expression = parts_at })
     else
       tbl
  | OnEveryPath consumed ->
     check_var_consumed_on_every_path place tbl name ty consumed

and check_var_consumed_on_every_path (place: place) (tbl: state_tbl) (name: identifier) (ty: ty) (consumed: int): state_tbl =
  (* What is the current state of this variable? *)
  let state: var_state = get_state place tbl name in
  (* Make a tuple with the variable's state, and the partitioned appearances. *)
  let tup = (state, partition consumed) in
  match tup with
  (*       State        Consumed  *)
  (* ---------------|-------------*)
  | (     Unconsumed,         Zero) -> (* Not yet consumed, not consumed. *)
     tbl
  | (     Unconsumed,          One) -> (* Not yet consumed, consumed once. *)
     consume_once place tbl name ty
  | (     Unconsumed,  MoreThanOne) -> (* Not yet consumed, consumed more than once. *)
     error_consumed_more_than_once place name ty;
     tbl
  | (   BorrowedRead,         Zero) -> (* Read borrowed, and not consumed. *)
     tbl
  | (   BorrowedRead,            _) -> (* Read borrowed, and consumed zero or more times. *)
     error_read_borrowed_and_consumed place name ty;
     tbl
  | (  BorrowedWrite,         Zero) -> (* Write borrowed, and not consumed. *)
     tbl
  | (  BorrowedWrite,            _) -> (* Write borrowed, and consumed zero or more times. *)
     error_write_borrowed_and_consumed place name ty;
     tbl
  | (       Consumed,         Zero) -> (* Already consumed, and unused. *)
     tbl
  | (       Consumed,            _) -> (* Already consumed, and consumed again zero or more times. *)
     (* Only a variable of a linear type is ever Consumed (`consume_once`). *)
     linearity_error place (AlreadyConsumed name)

and consume_once (place: place) (tbl: state_tbl) (name: identifier) (ty: ty): state_tbl =
  if universe_linear_ish (type_universe ty) then
    update_tbl place tbl name Consumed
  else
    tbl

and error_consumed_more_than_once (place: place) (name: identifier) (ty: ty) =
  if universe_linear_ish (type_universe ty) then
    linearity_error place (ConsumedMoreThanOnce name)
  else
    ()

and error_read_borrowed_and_consumed (place: place) (name: identifier) (ty: ty) =
  if universe_linear_ish (type_universe ty) then
    linearity_error place (ConsumedWhileBorrowedRead name)
  else
    ()

and error_write_borrowed_and_consumed (place: place) (name: identifier) (ty: ty) =
  if universe_linear_ish (type_universe ty) then
    linearity_error place (ConsumedWhileBorrowedWrite name)
  else
    ()

let check_expr (place: place) (tbl: state_tbl) (expr: texpr): state_tbl =
  (* For each variable in the table, check if the variable is used correctly in
     the expression. *)
  let vars: (identifier * ty) list = List.map (fun (name, ty, _) -> (name, ty)) (tbl_to_list tbl) in
  let f (tbl: state_tbl) (name, ty): state_tbl =
    check_var_in_expr place tbl name ty expr
  in
  Util.iter_with_context f tbl vars

(* Linearity checking *)

let rec check_stmt (place: place) (tbl: state_tbl) (stmt: tstmt): state_tbl =
  match stmt with
  | TSkip _ ->
     tbl
  | TLet (span, _, name, ty, body) ->
     let place: place = place_inside place span in
     (* Add an entry to the table. *)
     let tbl: state_tbl = add_entry place tbl name ty in
     let tbl: state_tbl = check_stmt place tbl body in
     (* Once we leave the scope, remove the variable we added. *)
     let tbl: state_tbl = remove_entry place tbl name in
     tbl
  | TLetTmp (name, ty, expr) ->
     (* Add an entry to the table. *)
     let tbl: state_tbl = add_entry place tbl name ty in
     let tbl: state_tbl = check_expr place tbl expr in
     tbl
  | TAssignTmp (name, expr) ->
    let tbl: state_tbl = check_expr place tbl expr in
    begin
      match get_entry tbl name with
      | Some (ty, state) ->
         begin
           match state with
           | Unconsumed | BorrowedRead | BorrowedWrite ->
              if universe_linear_ish (type_universe ty) then
                linearity_error place (AssignToUnconsumed name)
              else
                tbl
           | Consumed ->
              update_tbl place tbl name Unconsumed
         end
      | None ->
         tbl
    end
  | TDestructure (span, _, bindings, expr, body) ->
     let place: place = place_inside place span in
     (* First, check the expression. *)
     let tbl: state_tbl = check_expr place tbl expr in
     (* Iterate over the bidings, for each, add an entry to the table. Also,
        keep track of the names of the variables. *)
     let linear_names: identifier list =
       List.filter_map (fun (TypedBinding { rename; _ }) ->
           Some rename)
         bindings
     in
     let tbl: state_tbl =
       Util.iter_with_context
         (fun tbl (TypedBinding { rename; ty; _ }) ->
           add_entry place tbl rename ty)
         tbl
         bindings
     in
     let tbl: state_tbl = check_stmt place tbl body in
     (* Once we leave the scope, remove the linear variables we added. *)
     let tbl: state_tbl = remove_entries place tbl linear_names in
     tbl
  | TAssign (span, lvalue, rvalue) ->
     let place: place = place_inside place span in
     let tbl: state_tbl = check_expr place tbl lvalue in
     let tbl: state_tbl = check_expr place tbl rvalue in
     tbl
  | TAssignVar (span, name, rvalue) ->
     let place: place = place_inside place span in
     let tbl: state_tbl = check_expr place tbl rvalue in
     let name = local_name name in
     let (ty, state) = get_entry_or_fail place tbl name in
     begin
       match state with
       | Unconsumed ->
          if universe_linear_ish (type_universe ty) then
            linearity_error place (AssignToUnconsumed name)
          else
            tbl
       | BorrowedRead | BorrowedWrite ->
          (* Whatever the universe of the type: a reference into the old
             value would point into the new one. *)
          linearity_error place (AssignToBorrowed { name; state = reported_state state })
       | Consumed ->
          update_tbl place tbl name Unconsumed
     end
  | TInitialAssign (_, rvalue) ->
     let tbl: state_tbl = check_expr place tbl rvalue in
     tbl
  | TIf (span, cond, tb, fb) ->
     let place: place = place_inside place span in
     let tbl: state_tbl = check_expr place tbl cond in
     let true_tbl: state_tbl = check_stmt place tbl tb in
     let false_tbl: state_tbl = check_stmt place tbl fb in
     let _ = tables_are_consistent place IfBranching true_tbl false_tbl in
     true_tbl
  | TCase (span, expr, _, _, whens, _) ->
     let place: place = place_inside place span in
     let tbl: state_tbl = check_expr place tbl expr in
     let tbls: state_tbl list = check_whens place tbl whens in
     let _ = case_branch_tables_are_consistent place tbls in
     (match tbls with
      | first::rest ->
         let _ = rest in
         first
      | [] ->
         tbl)
  | TWhile (span, cond, body) ->
     let place: place = place_inside place span in
     let tbl: state_tbl = check_expr place tbl cond in
     check_loop_body place tbl body
  | TFor (span, _, start, final, body) ->
     let place: place = place_inside place span in
     let tbl: state_tbl = check_expr place tbl start in
     let tbl: state_tbl = check_expr place tbl final in
     check_loop_body place tbl body
  | TBorrow { original; mode; body; rename; ref_type; span; _ } ->
     let place: place = place_inside place span in
     let state_before_borrow: var_state = get_state place tbl original in
     let state_during_borrow: var_state =
       match (mode, state_before_borrow) with
       | (Read, (Unconsumed | BorrowedRead)) ->
          BorrowedRead
       | ((Write | Reborrow), Unconsumed) ->
          BorrowedWrite
       | (Read, (BorrowedWrite | Consumed)) | ((Write | Reborrow), (BorrowedRead | BorrowedWrite | Consumed)) ->
          linearity_error place (CannotBorrow { mode; name = original; state = reported_state state_before_borrow })
     in
     let tbl: state_tbl = update_tbl place tbl original state_during_borrow in
     (* Add the rename. *)
     let tbl: state_tbl = add_entry place tbl rename ref_type in
     (* Traverse the body. *)
     let tbl: state_tbl = check_stmt place tbl body in
     (* The borrow ends. A read borrow that began inside another read borrow
        leaves the variable borrowed: the outer borrow goes on. *)
     let tbl: state_tbl = update_tbl place tbl original state_before_borrow in
     (* Remove the rename from the table. *)
     let tbl: state_tbl = remove_entry place tbl rename in
     tbl
  | TBlock (span, a, b) ->
     let place: place = place_inside place span in
     let tbl: state_tbl = check_stmt place tbl a in
     let tbl: state_tbl = check_stmt place tbl b in
     tbl
  | TDiscarding (span, expr) ->
     let place: place = place_inside place span in
     let tbl: state_tbl = check_expr place tbl expr in
     tbl
  | TReturn (span, expr) ->
     let place: place = place_inside place span in
     let tbl: state_tbl = check_expr place tbl expr in
     (* Ensure that all variables are Consumed. *)
     let _ =
       List.map (fun (name, ty, state) ->
           if is_consumed state || need_not_be_consumed ty then
             ()
           else if universe_linear_ish (type_universe ty) then
             linearity_error place (NotConsumedAtReturn name)
           else
             ())
         (tbl_to_list tbl)
     in
     tbl

(* The body of a loop runs zero times or many times, so it leaves every
   variable in the state it found it in. *)
and check_loop_body (place: place) (tbl: state_tbl) (body: tstmt): state_tbl =
  let after: state_tbl = check_stmt place tbl body in
  (* The linear variables whose state the body changed, in one direction. A
     temporary that the body declared is only in the table after it. *)
  let changed_by_body (consumed_before: bool) (consumed_after: bool): identifier list =
    List.filter_map (fun (name, _, state_after) ->
        match get_entry tbl name with
        | Some (_, state_before) ->
           if is_consumed state_before = consumed_before && is_consumed state_after = consumed_after then
             Some name
           else
             None
        | None ->
           None)
      (tbl_to_list (trim_free after))
  in
  match (changed_by_body false true, changed_by_body true false) with
  | (_ :: _ as consumed_in_loop, _) ->
     linearity_error place (ConsumedInLoop consumed_in_loop)
  | ([], (_ :: _ as assigned_in_loop)) ->
     linearity_error place (AssignedInLoop assigned_in_loop)
  | ([], []) ->
     after

and check_whens (place: place) (tbl: state_tbl) (whens: typed_when list): state_tbl list =
  List.map (check_when place tbl) whens

and check_when (place: place) (tbl: state_tbl) (whn: typed_when): state_tbl =
  let TypedWhen (_, bindings, body) = whn in
  (* Add bindings to the table. *)
  let names: identifier list =
    List.map (fun (TypedBinding { rename; _ }) -> rename) bindings
  in
  let tbl: state_tbl =
    Util.iter_with_context
      (fun tbl (TypedBinding { rename; ty; _ }) ->
        add_entry place tbl rename ty)
      tbl
      bindings
  in
  (* Check the body. *)
  let tbl: state_tbl = check_stmt place tbl body in
  (* Once we leave the scope, remove the linear variables we added. *)
  let tbl: state_tbl = remove_entries place tbl names in
  tbl

let init_tbl (place: place) (tbl: state_tbl) (params: value_parameter list): state_tbl =
  let f (tbl: state_tbl) (ValueParameter (name, ty)): state_tbl =
    add_entry place tbl name ty
  in
  Util.iter_with_context f tbl params

let linearity_check (params: value_parameter list) (body: tstmt): unit =
  (* Initialize the state table to the empty table. *)
  let tbl: state_tbl = empty_tbl in
  (* Populate the table with the linear parameters. *)
  let tbl: state_tbl = init_tbl NoSpan tbl params in
  (* Traverse the code in execution order. *)
  let _ = check_stmt NoSpan tbl body in
  ()

(* Linearity checking of whole modules *)

type checked_module = CheckedModule of typed_module

let rec check_module_linearity (m: typed_module): checked_module Outcome.t =
  let (TypedModule (_, decls)) = m in
  Outcome.map (fun (_: unit list) -> CheckedModule m)
    (Outcome.traverse (fun decl -> Outcome.catch (fun () -> check_decl_linearity decl)) decls)

and check_decl_linearity (decl: typed_decl): unit =
  match decl with
  | TFunction (_, _, _, _, params, _, b, _) ->
     (linearity_check params b)
  | TInstance (_, _, _, _, _, methods, _) ->
     let _ = List.map check_method_linearity methods in
     ()
  | TConstant _ | TRecord _ | TUnion _ | TForeignFunction _ | TTypeClass _ ->
     ()

and check_method_linearity (TypedMethodDef (_, _, params, _, b)) =
  linearity_check params b

let as_typed_module (CheckedModule m): typed_module =
  m
