(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Stages
open Identifier
open Common
open Span

(* Lift anonymous borrows from expressions. *)

type acc_rec = { original: qident; rename: qident; mode: borrow_stmt_kind }
type acc = acc_rec list

let make_rename (supply: Supply.t) (original: qident): qident * Supply.t =
  let a = source_module_name original
  and b = original_name original
  and c = local_name original in
  let (fresh_id, supply) = Supply.next_borrow_rename supply in
  let id = string_of_int fresh_id in
  let b = make_ident ((ident_string b) ^ "_tmp_ref" ^ id) in
  let c = make_ident ((ident_string c) ^ "_tmp_ref" ^ id) in
  (make_qident (a, b, c), supply)

let make_mode (mode: borrowing_mode): borrow_stmt_kind =
  match mode with
  | ReadBorrow -> Read
  | WriteBorrow -> Write

let rec lift_borrows (supply: Supply.t) (expr: AstDP.aexpr) (acc: acc): AstDB.aexpr * acc * Supply.t =
  match expr with
  | AstDP.NilConstant -> (AstDB.NilConstant, acc, supply)
  | AstDP.BoolConstant b -> (AstDB.BoolConstant b, acc, supply)
  | AstDP.IntConstant i -> (AstDB.IntConstant i, acc, supply)
  | AstDP.FloatConstant f -> (AstDB.FloatConstant f, acc, supply)
  | AstDP.StringConstant s -> (AstDB.StringConstant s, acc, supply)
  | AstDP.Variable id -> (AstDB.Variable id, acc, supply)
  | AstDP.Temporary id -> (AstDB.Temporary id, acc, supply)
  | AstDP.BorrowExpr (mode, original) ->
    let (rename, supply) = make_rename supply original in
    let acc_rec = { original; rename; mode=make_mode mode } in
    let renamed_expr = AstDB.Variable rename in
    (renamed_expr, acc_rec :: acc, supply)
  | AstDP.Reborrow original ->
    let (rename, supply) = make_rename supply original in
    let acc_rec = { original; rename; mode = Reborrow } in
    let renamed_expr = AstDB.Variable rename in
    (renamed_expr, acc_rec :: acc, supply)
  | AstDP.FunctionCall (id, args) ->
    let (args', acc, supply) = lift_arglist supply args acc in
    let expr' = AstDB.FunctionCall (id, args') in
    (expr', acc, supply)
  | AstDP.ArithmeticExpression (op, left, right) ->
    let (left', acc, supply) = lift_borrows supply left acc in
    let (right', acc, supply) = lift_borrows supply right acc in
    (AstDB.ArithmeticExpression (op, left', right'), acc, supply)
  | AstDP.Comparison (op, left, right) ->
    let (left', acc, supply) = lift_borrows supply left acc in
    let (right', acc, supply) = lift_borrows supply right acc in
    (AstDB.Comparison (op, left', right'), acc, supply)
  | AstDP.Conjunction (left, right) ->
    let (left', acc, supply) = lift_borrows supply left acc in
    let (right', acc, supply) = lift_borrows supply right acc in
    (AstDB.Conjunction (left', right'), acc, supply)
  | AstDP.Disjunction (left, right) ->
    let (left', acc, supply) = lift_borrows supply left acc in
    let (right', acc, supply) = lift_borrows supply right acc in
    (AstDB.Disjunction (left', right'), acc, supply)
  | AstDP.Negation expr' ->
    let (expr'', acc, supply) = lift_borrows supply expr' acc in
    (AstDB.Negation expr'', acc, supply)
  | AstDP.IfExpression (cond, then', else') ->
    let (cond', acc, supply) = lift_borrows supply cond acc in
    let (then'', acc, supply) = lift_borrows supply then' acc in
    let (else'', acc, supply) = lift_borrows supply else' acc in
    (AstDB.IfExpression (cond', then'', else''), acc, supply)
  | AstDP.Embed (ty, str, exprs) ->
    let (exprs', acc, supply) = lift_borrows_list supply exprs acc in
    (AstDB.Embed (ty, str, exprs'), acc, supply)
  | AstDP.Deref expr' ->
    let (expr'', acc, supply) = lift_borrows supply expr' acc in
    (AstDB.Deref expr'', acc, supply)
  | AstDP.Typecast (expr', ty) ->
    let (expr'', acc, supply) = lift_borrows supply expr' acc in
    (AstDB.Typecast (expr'', ty), acc, supply)
  | AstDP.SizeOf ty -> (AstDB.SizeOf ty, acc, supply)
  | AstDP.SlotAccessor (subpath, name) ->
     let (subpath, acc, supply) = lift_borrows supply subpath acc in
     (AstDB.SlotAccessor (subpath, name), acc, supply)
  | AstDP.PointerSlotAccessor (subpath, name) ->
     let (subpath, acc, supply) = lift_borrows supply subpath acc in
     (AstDB.PointerSlotAccessor (subpath, name), acc, supply)
  | AstDP.ArrayIndex (subpath, expr) ->
     let (subpath, acc, supply) = lift_borrows supply subpath acc in
     let (expr, acc, supply) = lift_borrows supply expr acc in
     (AstDB.ArrayIndex (subpath, expr), acc, supply)

and lift_arglist (supply: Supply.t) (args: AstDP.abstract_arglist) (acc: acc): (AstDB.abstract_arglist * acc * Supply.t) =
  match args with
  | AstDP.Positional exprs ->
    let (exprs', acc', supply) = lift_borrows_list supply exprs acc in
    (AstDB.Positional exprs', acc', supply)
  | AstDP.Named pairs ->
    let (pairs', acc', supply) = lift_named_arg_pairs supply pairs acc in
    (AstDB.Named pairs', acc', supply)

and lift_named_arg_pairs (supply: Supply.t) (pairs: (identifier * AstDP.aexpr) list) (acc: acc): ((identifier * AstDB.aexpr) list * acc * Supply.t) =
  let ((acc, supply), l) = Util.map_with_context (fun ((acc, supply), pair) ->
                     let (name, expr) = pair in
                     let (expr, acc, supply) = lift_borrows supply expr acc in
                     let pair = (name, expr) in
                     ((acc, supply), pair))
                   (acc, supply)
                   pairs
  in
  (l, acc, supply)

and lift_borrows_list (supply: Supply.t) (exprs: AstDP.aexpr list) (acc: acc): (AstDB.aexpr list * acc * Supply.t) =
  let ((acc, supply), l) = Util.map_with_context (fun ((acc, supply), expr) ->
                     let (expr, acc, supply) = lift_borrows supply expr acc in
                     ((acc, supply), expr))
                   (acc, supply)
                   exprs
  in
  (l, acc, supply)

(* Wrap statements in borrow statements *)

let anonymous_region (supply: Supply.t): identifier * Supply.t =
  let (fresh_id, supply) = Supply.next_borrow_region supply in
  (make_ident ("_tmp_region_" ^ string_of_int fresh_id), supply)

let wrap_stmt_with_borrows (supply: Supply.t) (stmt: AstDB.astmt) (acc: acc): AstDB.astmt * Supply.t =
  let wrap_with_borrow ((stmt, supply): AstDB.astmt * Supply.t) (acc_rec: acc_rec): AstDB.astmt * Supply.t =
    let (region, supply) = anonymous_region supply in
    (AstDB.ABorrow {
        span = empty_span;
        original = original_name acc_rec.original;
        rename = original_name acc_rec.rename;
        region;
        body = stmt;
        mode = acc_rec.mode;
      }, supply)
  in
  List.fold_left wrap_with_borrow (stmt, supply) acc

(* Transform statements. *)

let rec transform_stmt (supply: Supply.t) (stmt: AstDP.astmt): AstDB.astmt * Supply.t =
  match stmt with
  | AstDP.ASkip span ->
     (AstDB.ASkip span, supply)
  | AstDP.ALet (span, mutability, name, ty, body) ->
     let (body, supply) = transform_stmt supply body in
     let stmt = AstDB.ALet (span, mutability, name, ty, body) in
     (stmt, supply)
  | AstDP.ADestructure (span, mutability, bindings, value, body) ->
     let (value, acc, supply) = lift_borrows supply value [] in
     let (body, supply) = transform_stmt supply body in
     let stmt = AstDB.ADestructure (span, mutability, bindings, value, body) in
     wrap_stmt_with_borrows supply stmt acc
  | AstDP.AAssign (span, path, value) ->
     let (path, acc, supply) = lift_borrows supply path [] in
     let (value, acc, supply) = lift_borrows supply value acc in
     let stmt = AstDB.AAssign (span, path, value) in
     wrap_stmt_with_borrows supply stmt acc
  | AstDP.AAssignVar (span, name, value) ->
     let (value, acc, supply) = lift_borrows supply value [] in
     let stmt = AstDB.AAssignVar (span, name, value) in
     wrap_stmt_with_borrows supply stmt acc
  | AstDP.AInitialAssign (name, ty, value) ->
     let (value, acc, supply) = lift_borrows supply value [] in
     let stmt = AstDB.AInitialAssign (name, ty, value) in
     wrap_stmt_with_borrows supply stmt acc
  | AstDP.LetTmp (name, value) ->
     let (value, acc, supply) = lift_borrows supply value [] in
     let stmt = AstDB.LetTmp (name, value) in
     wrap_stmt_with_borrows supply stmt acc
  | AstDP.AssignTmp (name, value) ->
     let (value, acc, supply) = lift_borrows supply value [] in
     let stmt = AstDB.AssignTmp (name, value) in
     wrap_stmt_with_borrows supply stmt acc
  | AstDP.AIf (span, cond, then', else') ->
     let (cond, acc, supply) = lift_borrows supply cond [] in
     let (then', supply) = transform_stmt supply then' in
     let (else', supply) = transform_stmt supply else' in
     let stmt = AstDB.AIf (span, cond, then', else') in
     wrap_stmt_with_borrows supply stmt acc
  | AstDP.AWhen (span, cond, body) ->
     let (cond, acc, supply) = lift_borrows supply cond [] in
     let (body, supply) = transform_stmt supply body in
     let stmt = AstDB.AWhen (span, cond, body) in
     wrap_stmt_with_borrows supply stmt acc
  | AstDP.ACase (span, expr, whens) ->
     let (expr, acc, supply) = lift_borrows supply expr [] in
     let (whens, supply) = transform_whens supply whens in
     let stmt = AstDB.ACase (span, expr, whens) in
     wrap_stmt_with_borrows supply stmt acc
  | AstDP.AWhile (span, cond, body) ->
     let (cond, acc, supply) = lift_borrows supply cond [] in
     let (body, supply) = transform_stmt supply body in
     let stmt = AstDB.AWhile (span, cond, body) in
     wrap_stmt_with_borrows supply stmt acc
  | AstDP.AFor { span; name; initial; final; body } ->
     let (initial, acc, supply) = lift_borrows supply initial [] in
     let (final, acc, supply) = lift_borrows supply final acc in
     let (body, supply) = transform_stmt supply body in
     let stmt = AstDB.AFor { span; name; initial; final; body } in
     wrap_stmt_with_borrows supply stmt acc
  | AstDP.ABlock (span, stmt1, stmt2) ->
     let (stmt1, supply) = transform_stmt supply stmt1 in
     let (stmt2, supply) = transform_stmt supply stmt2 in
     (AstDB.ABlock (span, stmt1, stmt2), supply)
  | AstDP.ADiscarding (span, expr) ->
     let (expr, acc, supply) = lift_borrows supply expr [] in
     let stmt = AstDB.ADiscarding (span, expr) in
     wrap_stmt_with_borrows supply stmt acc
  | AstDP.AReturn (span, expr) ->
     let (expr, acc, supply) = lift_borrows supply expr [] in
     let stmt = AstDB.AReturn (span, expr) in
     wrap_stmt_with_borrows supply stmt acc
  | AstDP.ABorrow { span; original; rename; region; body; mode } ->
     let (body, supply) = transform_stmt supply body in
     (AstDB.ABorrow { span; original; rename; region; body; mode }, supply)

and transform_whens (supply: Supply.t) (whens: AstDP.abstract_when list): AstDB.abstract_when list * Supply.t =
  Supply.map (fun supply (AstDP.AbstractWhen (id, bindings, body)) ->
    let (body, supply) = transform_stmt supply body in
    (AstDB.AbstractWhen (id, bindings, body), supply)
  ) supply whens

(* For an expression that is in no statement (the value of a constant). There
   is no statement to wrap in a borrow, so a borrow is an error. The supply is
   not returned: a name is taken from it only for a borrow, and then this
   raises. *)
let transform_expr (supply: Supply.t) (expr: AstDP.aexpr): AstDB.aexpr =
  let (expr, acc, _) = lift_borrows supply expr [] in
  if (List.length acc) > 0 then
    Error.Placed.generic_error Error.NoSpan BorrowExpressionNotAllowed
  else
    expr
