(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Span
open Common
open Type
open TypeSystem
open TypeBindings
open TypeMatch
open SignatureTypeAtUse
open TypeParser
open TypeParameter
open TypeParameters
open Region
open RegionMap
open LexEnv
open Env
open EnvTypes
open EnvUtils
open Stages.AstDB
open Stages.Tast
open TastUtil
open Stages.Linked
open Util
open Error

(* The place is where an error in the statement is reported. *)
type stmt_ctx = StmtCtx of module_name * env * region_map * typarams * ty * place

let update_rm (StmtCtx (mn, menv, _, typarams, rt, place)) (rm: region_map): stmt_ctx =
  StmtCtx (mn, menv, rm, typarams, rt, place)

(* The context of a statement whose span is `span`, inside the statement of `ctx`. *)
let stmt_at (StmtCtx (mn, menv, rm, typarams, rt, outer)) (span: span): stmt_ctx =
  StmtCtx (mn, menv, rm, typarams, rt, place_inside outer span)

(* Since the extraction pass has already happened, we can simplify the call to
   `parse_type` by passing an empty list of local type signatures. *)
let parse_typespec (place: place) (env: env) (rm: region_map) (typarams: typarams) (ty: qtypespec): ty =
  parse_type place env [] rm typarams ty

let infer_expr (place: place) (module_name: module_name) (env: env) (rm: region_map) (typarams: typarams) (lexenv: lexenv) (expr: aexpr): texpr =
  let ctx: TypeCheckExpr.expr_ctx = TypeCheckExpr.make_ctx place module_name env rm typarams lexenv in
  TypeCheckExpr.infer ctx expr

let check_expr (place: place) (module_name: module_name) (env: env) (rm: region_map) (typarams: typarams) (lexenv: lexenv) (expr: aexpr) (expected: ty): texpr =
  let ctx: TypeCheckExpr.expr_ctx = TypeCheckExpr.make_ctx place module_name env rm typarams lexenv in
  TypeCheckExpr.check ctx expr expected

let check_annotated_expr (place: place) (module_name: module_name) (env: env) (rm: region_map) (typarams: typarams) (lexenv: lexenv) (expr: aexpr) (annotation: ty): texpr =
  let ctx: TypeCheckExpr.expr_ctx = TypeCheckExpr.make_ctx place module_name env rm typarams lexenv in
  TypeCheckExpr.check_annotated ctx expr annotation

let is_boolean = function
  | Boolean -> true
  | Unit | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _ | ReadRef _
  | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _ -> false

type case_mode =
  | NormalCaseMode
  | ReadRefCaseMode of ref_region
  | WriteRefCaseMode of ref_region

let check_var_doesnt_collide_with_decl (ctx: stmt_ctx) (name: identifier) =
  let (StmtCtx (module_name, env, _, _, _, place)) = ctx in
  let sname: sident = make_sident module_name name in
  match get_decl_by_name place env sname with
  | Some _ ->
     generic_error place (VarCollidesWithDecl name)
  | None ->
     ()

(* In the body of a borrow, a built-in type of the region's name hides the
   region, and the region hides a type parameter or a declared type of that
   name (TypeParser.parse_type, #153). So the region cannot have a name that a
   declaration cannot have, nor the name of a type parameter. The module being
   checked is in the environment. *)
let check_region_name_is_free (ctx: stmt_ctx) (region: identifier): unit =
  let (StmtCtx (module_name, env, _, typarams, _, place)) = ctx in
  match get_typaram typarams region with
  | Some _ ->
     declaration_error place (RegionNameIsTypeParameter region)
  | None ->
     Option.iter (fun (ModRec { id; _ }) -> name_must_be_free place env id region)
       (get_module_by_name env module_name)

(* Whether `stmt` assigns a temporary, which is what `LiftControlPass` turns
   every assignment into. `DesugarBorrows` wraps such a statement in the
   anonymous borrows of its lvalue. *)
let rec assigns_lifted_temporary (stmt: astmt): bool =
  match stmt with
  | AAssign (_, _, Temporary _) | AAssignVar (_, _, Temporary _) ->
     true
  | ABorrow { body; _ } ->
     assigns_lifted_temporary body
  | ASkip _ | ALet _ | ADestructure _ | AInitialAssign _ | AIf _ | AWhen _ | ACase _
  | AWhile _ | AFor _ | ABlock _ | ADiscarding _ | AReturn _ | LetTmp _ | LetLoopBound _
  | AssignTmp _
  | AAssign (_, _, (NilConstant | BoolConstant _ | IntConstant _ | FloatConstant _
      | StringConstant _ | Variable _ | FunctionCall _ | ArithmeticExpression _
      | ArithmeticNegation _ | Comparison _ | Conjunction _ | Disjunction _ | Negation _
      | IfExpression _ | Embed _ | Deref _ | Typecast _ | SizeOf _ | SlotAccessor _
      | PointerSlotAccessor _ | ArrayIndex _)) | AAssignVar (_, _, (NilConstant
      | BoolConstant _ | IntConstant _ | FloatConstant _ | StringConstant _
      | Variable _ | FunctionCall _ | ArithmeticExpression _ | ArithmeticNegation _
      | Comparison _ | Conjunction _ | Disjunction _ | Negation _ | IfExpression _ | Embed _ | Deref _
      | Typecast _ | SizeOf _ | SlotAccessor _ | PointerSlotAccessor _
      | ArrayIndex _)) ->
     false

(* The table with the temporary on the right-hand side of an assignment,
   declared with the type of the lvalue. The statement that computes the
   temporary is checked after the assignment (see `augment_block`), against
   that type. *)
let declare_assigned_temporary (place: place) (lexenv: lexenv) (rvalue: aexpr) (ty: ty): lexenv =
  match rvalue with
  | Temporary name ->
     push_var place lexenv name ty (VarLocal Mutable)
  | NilConstant | BoolConstant _ | IntConstant _ | FloatConstant _ | StringConstant _
  | Variable _ | FunctionCall _ | ArithmeticExpression _ | ArithmeticNegation _ | Comparison _
  | Conjunction _ | Disjunction _ | Negation _ | IfExpression _ | Embed _ | Deref _ | Typecast _
  | SizeOf _ | SlotAccessor _ | PointerSlotAccessor _ | ArrayIndex _ ->
     lexenv

(* The supply numbers the regions of the borrow statements, in the order the
   statements are checked. That order is not the order of the source: the
   `else` branch of an `if` is checked before the `then` branch, and an
   assignment before the statement that computes its value. Which of two
   errors is reported depends on the order, so it stays.

   The table of variables goes the same way. A statement is checked with the
   table that the statement checked before it returned, and returns its own. A
   statement that binds a name (`let`, `for`, `borrow`, a destructuring `let`,
   a `when`) checks its body with the name in the table and returns the table
   of the body without it. What a statement leaves in the table are
   temporaries: the one an assignment declares for its value, which the
   statement that computes the value then finds, and the ones that hold the
   expressions lifted out of control statements. They stay to the end of the
   function, also past the end of the block they are in, and the `then`
   branch of an `if` is checked with the temporaries of its `else` branch. A
   program cannot write the name of a temporary, so it cannot tell. *)
let rec augment_stmt (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (stmt: astmt): tstmt * lexenv * Supply.t =
  match stmt with
  | ASkip span ->
     (TSkip span, lexenv, supply)
  | ALet (span, mut, name, ty, body) ->
     augment_let ctx lexenv supply span mut name ty body
  | LetTmp (name, value) ->
     augment_let_tmp ctx lexenv supply name value
  | LetLoopBound (name, bound, value) ->
     augment_let_loop_bound ctx lexenv supply name bound value
  | AssignTmp (name, value) ->
     augment_assign_tmp ctx lexenv supply name value
  | ADestructure (span, mut, bindings, value, body) ->
     augment_destructure ctx lexenv supply span mut bindings value body
  | AAssign (span, lvalue, rvalue) ->
     augment_assign ctx lexenv supply span lvalue rvalue
  | AAssignVar (span, name, rvalue) ->
     augment_assign_var ctx lexenv supply span name rvalue
  | AInitialAssign (name, ty, rvalue) ->
     augment_initial_assign ctx lexenv supply name ty rvalue
  | AIf (span, c, t, f) ->
     augment_if ctx lexenv supply span c t f
  | AWhen (span, c, t) ->
     augment_if_without_else ctx lexenv supply span c t
  | ACase (span, expr, whens) ->
     augment_case ctx lexenv supply span expr whens
  | AWhile (span, c, body) ->
     augment_while ctx lexenv supply span c body
  | AFor { span; name; initial; final; body; } ->
     augment_for ctx lexenv supply span name initial final body
  | ABorrow { span; original; rename; declared_reference; region; body; mode; } ->
     augment_borrow ctx lexenv supply span original rename declared_reference region body mode
  | ABlock (span, f, r) ->
     augment_block ctx lexenv supply span f r
  | ADiscarding (span, e) ->
     augment_discarding ctx lexenv supply span e
  | AReturn (span, e) ->
     augment_return ctx lexenv supply span e

and augment_let (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (mut: mutability) (name: identifier) (ty: qtypespec) (body: astmt): tstmt * lexenv * Supply.t =
  let ctx = stmt_at ctx span in
  let (StmtCtx (_, env, rm, typarams, _, place)) = ctx in
  (let _ = check_var_doesnt_collide_with_decl ctx name in
      let ty = parse_typespec place env rm typarams ty in
      (* The body starts with the statement that assigns the initial value
         (LiftControlPass). The variable has no value before that statement
         has run, so nothing in it can name the variable. *)
      let lexenv = push_var_awaiting_initial_value place lexenv name ty (VarLocal mut) in
      let (body', lexenv, supply) = augment_stmt ctx lexenv supply body in
      (TLet (span, mut, name, ty, body'), remove_var lexenv name, supply))

and augment_let_tmp (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (name: identifier) (value: aexpr): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _, place)) = ctx in
  (match get_var lexenv name with
   | Some (ty, _) ->
      (* The temporary holds the right-hand side of an assignment, which
         has declared it with the type of its lvalue. That type annotates the
         right-hand side, as the type of a `let` does its value: a write
         reference becomes a read reference here too (#30). *)
      let value = check_annotated_expr place module_name env rm typarams lexenv value ty in
      (TLetTmp (name, ty, value), lexenv, supply)
   | None ->
      let value = infer_expr place module_name env rm typarams lexenv value in
      let ty = get_type place value in
      let lexenv = push_var place lexenv name ty (VarLocal Mutable) in
      (TLetTmp (name, ty, value), lexenv, supply))

(* A bound of a `for` loop has the type Index, which an integer literal takes. *)
and augment_let_loop_bound (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (name: identifier) (bound: loop_bound) (value: aexpr): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _, place)) = ctx in
  let expr_ctx: TypeCheckExpr.expr_ctx = TypeCheckExpr.make_ctx place module_name env rm typarams lexenv in
  let value = TypeCheckExpr.infer_with_expected expr_ctx value index_type in
  if LiteralTyping.literal_matches place index_type value || equal_ty (get_type place value) index_type then
    (TLetTmp (name, index_type, value), push_var place lexenv name index_type (VarLocal Mutable), supply)
  else
    type_error place (ForBoundNotIndex { bound; ty = get_type place value })

and augment_assign_tmp (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (name: identifier) (value: aexpr): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _, place)) = ctx in
  let value = infer_expr place module_name env rm typarams lexenv value in
  let ty = get_temporary_type place lexenv name in
  let _ = expect_same_type (env, module_name, place) ty (get_type place value) in
  (TAssignTmp (name, value), lexenv, supply)

and augment_destructure (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (mut: mutability) (bindings: qbinding list) (value: aexpr) (body: astmt): tstmt * lexenv * Supply.t =
  let ctx = stmt_at ctx span in
  let (StmtCtx (module_name, env, rm, typarams, _, place)) = ctx in
  (let _ =
        List.map (fun (Stages.Ast.QBinding { rename; _ }) -> check_var_doesnt_collide_with_decl ctx rename) bindings
      in
      let value' = infer_expr place module_name env rm typarams lexenv value in
      (* Check: the value must be a public record type *)
      let rec_ty = get_type place value' in
      (match rec_ty with
       | NamedType (_, Id.UnionTypeId _, _, _) ->
          type_error place (DestructureNonRecord rec_ty)
       | NamedType (name, (Id.RecordTypeId _ as id), _, u) ->
          let (source_module, vis, record_typarams, slots) = TypeCheckExpr.get_record_definition place env id rec_ty in
          let orig_type = NamedType (
                              make_qident (source_module, original_name name, original_name name),
                              id,
                              List.map (fun tp -> TyVar (typaram_to_tyvar tp)) (typarams_as_list record_typarams),
                              u
                            )
          in
          let typebindings = match_type (env, module_name, place) (signature_type_at_use orig_type) rec_ty in
          let public: bool =
            match vis with
            | TypeVisPublic -> true
            | TypeVisOpaque | TypeVisPrivate -> false
          in
          if public || (module_name = source_module) then
            (* Find the set of slot names and the set of binding names, and compare them *)
            let binding_names = List.map (fun (Stages.Ast.QBinding { name; _}) -> name) bindings
            and slot_names = List.map (fun (TypedSlot (n, _)) -> n) slots in
            if ident_set_eq binding_names slot_names then
              let bindings': (identifier * qtypespec * ty * identifier) list =
                group_bindings_slots bindings slots
              in
              let bindings'': (identifier * ty * ty * identifier) list
                = List.map (fun (n, ty, actual, rename) -> (n, parse_typespec place env rm typarams ty, replace_variables typebindings actual, rename)) bindings'
              in
              let newvars: (identifier * ty * var_source) list =
                List.map (fun (_, ty, actual, rename) ->
                    let _ = expect_same_type (env, module_name, place) ty actual in
                    (rename, ty, VarLocal mut))
                  bindings''
              in
              let lexenv = push_vars place lexenv newvars in
              let (body', lexenv, supply) = augment_stmt ctx lexenv supply body in
              (TDestructure (
                  span,
                  mut,
                  List.map (fun (name, _, ty, rename) -> TypedBinding { name; ty; rename; }) bindings'',
                  value',
                  body'
                ), remove_vars lexenv (List.map (fun (n, _, _) -> n) newvars), supply)
            else
              type_error place (DestructureWrongSlots rec_ty)
          else
            type_error place (DestructureNotPublic rec_ty)
       | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _
       | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _
       | Pointer _ | FnPtr _ ->
          type_error place (DestructureNonRecord rec_ty)))

and augment_assign (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (lvalue: aexpr) (rvalue: aexpr): tstmt * lexenv * Supply.t =
  let ctx = stmt_at ctx span in
  let (StmtCtx (module_name, env, rm, typarams, _, place)) = ctx in
  ((* Augment the lvalue. *)
      let lvalue: texpr = infer_expr place module_name env rm typarams lexenv lvalue in
      (* The type of the lvalue must be a read reference. *)
      let lvalue_ty: ty = get_type place lvalue in
      let pointed_ty: ty = begin
          match lvalue_ty with
          | WriteRef (ty, _) ->
             ty
          | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
          | RegionTy _ | ReadRef _ | Span _ | SpanMut _ | TyVar _ | Address _
          | Pointer _ | FnPtr _ ->
             generic_error place (AssignToNonMutableReference lvalue_ty)
        end
      in
      (* Augment the rvalue. *)
      let lexenv = declare_assigned_temporary place lexenv rvalue pointed_ty in
      let rvalue: texpr = infer_expr place module_name env rm typarams lexenv rvalue in
      (* The pointed to type and the type of the rvalue must match. *)
      let _ = expect_value_of_type (env, module_name, place) pointed_ty rvalue in
      (* The pointed-to type must be in the free universe. *)
      let _ =
        match type_universe pointed_ty with
        | FreeUniverse ->
           ()
        | LinearUniverse | TypeUniverse | RegionUniverse ->
           linearity_error place (AssignToNonFreePath pointed_ty)
      in
      (* Construct the assignment. *)
      (TAssign (span, lvalue, rvalue), lexenv, supply))

and augment_assign_var (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (name: qident) (rvalue: aexpr): tstmt * lexenv * Supply.t =
  let ctx = stmt_at ctx span in
  let (StmtCtx (module_name, env, rm, typarams, _, place)) = ctx in
  ((* Get the type of the lvalue. *)
      let lvalue_ty: ty = begin
          (* A constant is not in the lexical environment: it is looked up as
             an expression looks a name up, so that assigning to one is said
             to be that, and not an unknown name. *)
          let variable: (ty * variable_source) option = get_variable place env lexenv name in
          let name: identifier = local_name name in
        match variable with
        | (Some (orig_ty, src)) -> begin
            let _ =
              match src with
              | VarLexical (VarLocal Mutable) ->
                 ()
              | VarLexical (VarLocal Immutable) ->
                 generic_error place (CantAssignToImmutableVar name)
              | VarConstant ->
                 generic_error place CannotAssignToConstant
              | VarLexical VarParam ->
                 generic_error place CannotAssignToParameter
            in
            orig_ty
          end
        | None ->
           generic_error place (UnknownName { kind = VariableName; name })
        end
      in
      (* Augment the rvalue. *)
      let lexenv = declare_assigned_temporary place lexenv rvalue lvalue_ty in
      let rvalue: texpr = infer_expr place module_name env rm typarams lexenv rvalue in
      (* Types must match. *)
      let _ = expect_value_of_type (env, module_name, place) lvalue_ty rvalue in
      (* Construct the assignment. *)
      (TAssignVar (span, name, rvalue), lexenv, supply))

and augment_initial_assign (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (name: qident) (ty: qtypespec) (rvalue: aexpr): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _, place)) = ctx in
  (* The type of the `let` annotates its value. *)
  let ty = parse_typespec place env rm typarams ty in
  (* The `let` declared its variable without a value, so the initial value
     does not see the variable, and neither does an anonymous borrow around
     this statement. The statements after this one see it. *)
  let rvalue: texpr = check_annotated_expr place module_name env rm typarams lexenv rvalue ty in
  (TInitialAssign (name, rvalue), mark_var_initialized lexenv (local_name name), supply)

and augment_if (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (c: aexpr) (t: astmt) (f: astmt): tstmt * lexenv * Supply.t =
  let ctx = stmt_at ctx span in
  let (StmtCtx (module_name, env, rm, typarams, _, place)) = ctx in
  (let c' = infer_expr place module_name env rm typarams lexenv c in
      if is_boolean (get_type place c') then
        let (f, lexenv, supply) = augment_stmt ctx lexenv supply f in
        let (t, lexenv, supply) = augment_stmt ctx lexenv supply t in
        (TIf (span, c', t, f), lexenv, supply)
      else
        type_error place (ConditionNotBoolean { condition = IfStatementCondition; ty = get_type place c' }))

and augment_if_without_else (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (c: aexpr) (t: astmt): tstmt * lexenv * Supply.t =
  let ctx = stmt_at ctx span in
  let (StmtCtx (module_name, env, rm, typarams, _, place)) = ctx in
  (let c = infer_expr place module_name env rm typarams lexenv c in
      if is_boolean (get_type place c) then
        let (t, lexenv, supply) = augment_stmt ctx lexenv supply t in
        (TIf (span, c, t, TSkip span), lexenv, supply)
      else
        type_error place (ConditionNotBoolean { condition = IfStatementCondition; ty = get_type place c }))

and augment_while (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (c: aexpr) (body: astmt): tstmt * lexenv * Supply.t =
  let ctx = stmt_at ctx span in
  let (StmtCtx (module_name, env, rm, typarams, _, place)) = ctx in
  (let c' = infer_expr place module_name env rm typarams lexenv c in
      if is_boolean (get_type place c') then
        let (body, lexenv, supply) = augment_stmt ctx lexenv supply body in
        (TWhile (span, c', body), lexenv, supply)
      else
        type_error place (ConditionNotBoolean { condition = WhileStatementCondition; ty = get_type place c' }))

and augment_for (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (name: identifier) (initial: aexpr) (final: aexpr) (body: astmt): tstmt * lexenv * Supply.t =
  let ctx = stmt_at ctx span in
  let (StmtCtx (module_name, env, rm, typarams, _, place)) = ctx in
  (* Both bounds are temporaries of the type Index (augment_let_loop_bound). *)
  (let i' = infer_expr place module_name env rm typarams lexenv initial
      and f' = infer_expr place module_name env rm typarams lexenv final in
      let lexenv = push_var place lexenv name index_type (VarLocal Immutable) in
      let (b', lexenv, supply) = augment_stmt ctx lexenv supply body in
      (TFor (span, name, i', f', b'), remove_var lexenv name, supply))

and augment_borrow (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (original: identifier) (rename: identifier) (declared_reference: (borrowing_mode * qtypespec) option) (region: identifier) (body: astmt) (mode: borrow_stmt_kind): tstmt * lexenv * Supply.t =
  let ctx = stmt_at ctx span in
  let (StmtCtx (module_name, env, rm, typarams, _, place)) = ctx in
  (let _ = check_var_doesnt_collide_with_decl ctx rename in
   let _ = check_region_name_is_free ctx region in
      (match get_variable place env lexenv (make_qident (module_name, original, original)) with
       | Some (_, VarConstant) ->
          type_error place (CannotBorrowConstant original)
       | Some (orig_ty, VarLexical src) ->
          (* Check we can borrow the variable. *)
          let _ =
            match (src, mode) with
            | (_, Read) ->
               (* Anything can be borrowed immutably. *)
               ()
            | (VarLocal Immutable, Write) ->
               (* Immutable variables cannot be borrowed mutably. *)
               type_error place (CannotBorrowImmutableVarMutably original)
            | (VarParam, Write) ->
               (* Parameters cannot be borrowed mutably, unless they're mutable references. *)
               let is_mut = begin
                 match orig_ty with
                 | WriteRef _ -> true
                 | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat
                 | NamedType _ | RegionTy _ | ReadRef _ | Span _ | SpanMut _
                 | TyVar _ | Address _ | Pointer _ | FnPtr _ -> false
               end
               in
               if is_mut then
                 ()
               else
                 type_error place (CannotBorrowParamMutably { name = original; ty = orig_ty })
            | (VarLocal Mutable, Write) | (_, Reborrow) ->
               (* And anything else is fine. *)
               ()
          in
          let (region_obj, supply) = fresh_region region supply in
          let refty =
            (match mode with
             | Read ->
                ReadRef (orig_ty, RefRegion region_obj)
             | Write ->
                WriteRef (orig_ty, RefRegion region_obj)
             | Reborrow ->
                let pointed_ty = begin
                    match orig_ty with
                    | WriteRef (pointed_ty, _) ->
                       pointed_ty
                    | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat
                    | NamedType _ | RegionTy _ | ReadRef _ | Span _ | SpanMut _
                    | TyVar _ | Address _ | Pointer _ | FnPtr _ ->
                       generic_error place (ReborrowOfNonMutableReference orig_ty)
                  end
                in
                WriteRef (pointed_ty, RefRegion region_obj))
          in
          (* The reference has the type the statement declares for it. Only a
             borrow the user wrote declares one. *)
          let origin: borrow_origin =
            match declared_reference with
            | Some (declared_mode, declared_referent) ->
               let declared_referent: ty = parse_typespec place env rm typarams declared_referent in
               let declared_refty: ty =
                 match declared_mode with
                 | ReadBorrow -> ReadRef (declared_referent, RefRegion region_obj)
                 | WriteBorrow -> WriteRef (declared_referent, RefRegion region_obj)
               in
               if equal_ty declared_refty refty then
                 WrittenBorrow
               else
                 type_error place (TypeMismatch { expected = declared_refty; actual = refty })
            | None ->
               AnonymousBorrow
          in
          let lexenv = push_var place lexenv rename refty (VarLocal Immutable) in
          let rm' = add_region place rm region region_obj in
          let ctx' = update_rm ctx rm' in
          let (body, lexenv, supply) = augment_stmt ctx' lexenv supply body in
          (TBorrow {
              span=span;
              original=original;
              rename=rename;
              region=region;
              orig_type=orig_ty;
              ref_type=refty;
              body=body;
              mode=mode;
              origin=origin
            }, remove_var lexenv rename, supply)
       | None ->
          generic_error place (UnknownName { kind = VariableName; name = original })))

and augment_block (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (f: astmt) (r: astmt): tstmt * lexenv * Supply.t =
  (* A block that `LiftControlPass` made has the span of the statement it
     lifted an expression out of, and the statement that computes the
     expression has none of its own. *)
  let ctx = stmt_at ctx span in
  if assigns_lifted_temporary r then
    (* `f` computes the temporary that `r` assigns. `r` is checked first
       because it gives the temporary its type, and the type of a path is
       only known inside the borrows around `r`. *)
    let (b, lexenv, supply) = augment_stmt ctx lexenv supply r in
    let (a, lexenv, supply) = augment_stmt ctx lexenv supply f in
    (TBlock (span, a, b), lexenv, supply)
  else
    let (a, lexenv, supply) = augment_stmt ctx lexenv supply f in
    let (b, lexenv, supply) = augment_stmt ctx lexenv supply r in
    (TBlock (span, a, b), lexenv, supply)

and augment_discarding (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (e: aexpr): tstmt * lexenv * Supply.t =
  let ctx = stmt_at ctx span in
  let (StmtCtx (module_name, env, rm, typarams, _, place)) = ctx in
  (let e' = infer_expr place module_name env rm typarams lexenv e in
      let u = type_universe (get_type place e') in
      match u with
      | LinearUniverse | TypeUniverse ->
         type_error place (DiscardLinear u)
      | FreeUniverse | RegionUniverse ->
         (TDiscarding (span, e'), lexenv, supply))

and augment_return (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (e: aexpr): tstmt * lexenv * Supply.t =
  let ctx = stmt_at ctx span in
  let (StmtCtx (module_name, env, rm, typarams, rt, place)) = ctx in
  (let e' = check_expr place module_name env rm typarams lexenv e rt in
      (TReturn (span, e'), lexenv, supply))

and augment_case (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (expr: aexpr) (whens: abstract_when list): tstmt * lexenv * Supply.t =
  let ctx = stmt_at ctx span in
  let (StmtCtx (module_name, env, rm, typarams, _, place)) = ctx in
  (* Type checking a case statement:

     1. Ensure the value is of a union type (or reference to a union).
     2. Ensure the union type is public or it is defined in this module.
     3. Ensure the set of case names in the case statement equals the set of cases in the union definition.
     4. Iterate over the cases, and ensure the bindings are correct.
   *)
  (let expr' = infer_expr place module_name env rm typarams lexenv expr in
      let ty = get_type place expr' in
      let ty, mode =
        (match ty with
         | NamedType _ ->
            (ty, NormalCaseMode)
         | ReadRef (ty, r) ->
            (ty, ReadRefCaseMode r)
         | WriteRef (ty, r) ->
            (ty, WriteRefCaseMode r)
         | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _ | Span _
         | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _ ->
            type_error place (CaseNotUnionOrReference ty))
      in
      let case_ref: case_ref =
        (match mode with
         | NormalCaseMode -> CasePlain
         | ReadRefCaseMode _ -> CaseRef
         | WriteRefCaseMode _ -> CaseRef)
      in
      let (union_id, tyargs, union_ty, cases) = get_union_type_definition place module_name env ty in
      let typebindings = match_type (env, module_name, place) (signature_type_at_use union_ty) ty in
      let case_names = List.map (fun (TypedCase (n, _)) -> n) (List.map union_case_to_typed_case cases) in
      (* A `when` names a case by a name the module has for it: its own name in
         the module of the union, or the nickname it is imported under (#50). *)
      let case_qnames: qident list =
        List.map (fun (case: union_case_decl) -> make_qident (module_name_from_id place env case.mod_id, case.name, case.name)) cases
      in
      let names_a_case_of_the_union (AbstractWhen (n, _, _)): bool =
        List.exists (equal_qident n) case_qnames
      in
      let when_names = List.map (fun (AbstractWhen (n, _, _)) -> original_name n) whens in
      if List.for_all names_a_case_of_the_union whens && ident_set_eq case_names when_names then
        (* Group the cases and whens *)
        let whens' = group_cases_whens cases whens in
        let (whens'', lexenv, supply) = augment_whens ctx lexenv supply typebindings whens' mode in
        (TCase (span, expr', union_id, tyargs, whens'', case_ref), lexenv, supply)
      else
        type_error place CaseNonExhaustive)

and get_union_type_definition (place: place) (importing_module: module_name) (env: env) (ty: ty): (Id.union_id * ty list * ty * union_case_decl list) =
  let (name, tyargs): qident * ty list = (match ty with
                      | NamedType (n, _, a, _) ->
                         (n, a)
                      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat
                      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _
                      | TyVar _ | Address _ | Pointer _ | FnPtr _ ->
                         type_error place (CaseNonUnion ty)) in
  match get_decl_by_name place env (qident_to_sident name) with
  | Some (Union { id; vis; name; mod_id; typarams; universe; _ }) ->
     let module_name = module_name_from_id place env mod_id in
     let public: bool =
       match vis with
       | TypeVisPublic -> true
       | TypeVisOpaque | TypeVisPrivate -> false
     in
     if public || (importing_module = module_name) then
       let n = make_qident (module_name, name, name)
       and args = List.map (fun tp -> TyVar (typaram_to_tyvar tp)) (typarams_as_list typarams)
       in
       let ty = NamedType (n, Id.UnionTypeId id, args, universe)
       and cases = get_union_cases env id in
       (id, tyargs, ty, cases)
     else
       type_error place CaseNonPublic
  | None | Some (Constant _ | Record _ | UnionCase _ | Function _ | TypeClass _
      | TypeClassMethod _) ->
     type_error place (CaseNonUnion ty)

and group_cases_whens (cases: union_case_decl list) (whens: abstract_when list): (typed_case * abstract_when) list =
  List.map (fun (TypedCase (n, s)) -> (TypedCase (n, s), List.find (fun (AbstractWhen (n', _, _)) -> equal_identifier n (original_name n')) whens))
    (List.map union_case_to_typed_case cases)

and group_bindings_slots (bindings: qbinding list) (slots: typed_slot list): (identifier * qtypespec * ty * identifier) list =
  let f (binding: qbinding): (identifier * qtypespec * ty * identifier) =
    let Stages.Ast.QBinding { name; ty; rename; } = binding in
    let (TypedSlot (_, ty')) = List.find (fun (TypedSlot (n', _)) -> equal_identifier name n') slots in
    (name, ty, ty', rename)
  in
  List.map f bindings

(* The branches are checked from the first to the last, each with the table and
   the supply that the one before it returned. *)
and augment_whens (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (typebindings: type_bindings) (whens: (typed_case * abstract_when) list) (mode: case_mode): typed_when list * lexenv * Supply.t =
  match whens with
  | [] ->
     ([], lexenv, supply)
  | (c, w) :: rest ->
     let (w, lexenv, supply) = augment_when ctx lexenv supply typebindings w c mode in
     let (rest, lexenv, supply) = augment_whens ctx lexenv supply typebindings rest mode in
     (w :: rest, lexenv, supply)

and augment_when (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (typebindings: type_bindings) (w: abstract_when) (c: typed_case) (mode: case_mode): typed_when * lexenv * Supply.t =
  let (StmtCtx (_, menv, rm, typarams, _, place)) = ctx in
  let (AbstractWhen (name, bindings, body)) = w
  and (TypedCase (_, slots)) = c in
  (* Check the set of binding names is the same as the set of slots *)
  let binding_names = List.map (fun (Stages.Ast.QBinding { name; _ }) -> name) bindings
  and slot_names = List.map (fun (TypedSlot (n, _)) -> n) slots in
  if ident_set_eq binding_names slot_names then
    (* Check the type of each binding matches the type of the slot *)
    let bindings' = group_bindings_slots bindings slots in
    let bindings'' = List.map (fun (n, ty, actual, rename) -> (n, parse_typespec place menv rm typarams ty, replace_variables typebindings actual, rename)) bindings' in
    let newvars = List.map (fun (_, user_ty, decl_ty, rename) ->
                      (* Depending on what 'mode' of case statement this is,
                         we may have to modify the slot type. If we're just
                         accessing a union by value, we don't have to do
                         anything, but if we're accessing a union by
                         reference, we don't have access to the slot
                         _values_, we instead get _references_ to those
                         values. Just like when doing `val->x` with `val`
                         having a reference-to-record type gives us a
                         reference to the field `x`, case statements on
                         references give us references to their interior
                         values. *)
                      let decl_ty =
                        (match mode with
                         | NormalCaseMode ->
                            decl_ty
                         | ReadRefCaseMode r ->
                            ReadRef (decl_ty, r)
                         | WriteRefCaseMode r ->
                            WriteRef (decl_ty, r))
                      in
                      if equal_ty decl_ty user_ty then
                        (rename, decl_ty, VarLocal Immutable)
                      else
                        type_error place (SlotWrongType {
                            name = rename;
                            expected = decl_ty;
                            actual = user_ty;
                          }))
                    bindings'' in
    let lexenv = push_vars place lexenv newvars in
    let (body', lexenv, supply) = augment_stmt ctx lexenv supply body in
    (TypedWhen (original_name name, List.map (fun (name, _, ty, rename) -> TypedBinding { name; ty; rename}) bindings'', body'), remove_vars lexenv (List.map (fun (n, _, _) -> n) newvars), supply)
  else
    type_error place (CaseWrongSlots { binding_names; slot_names })

let validate_constant_expression (place: place) (expr: texpr): unit =
  if TastUtil.is_constant_expression expr then
    ()
  else
    type_error place ExpressionNotConstant

(* [place] is the span of the definition. *)
let rec augment_decl (module_name: module_name) (kind: module_kind) (env: env) (module_constants: identifier list) (supply: Supply.t) (place: place) (decl: linked_definition): typed_decl * Supply.t =
  let rm = empty_region_map in
  match decl with
  | LConstant (decl_id, vis, name, ty, expr, doc) ->
     (* The value names only the constants defined before this one (#142),
        which rules out cycles: a constant is a C macro, and C does not expand
        a macro inside itself. *)
     let rec this_constant_and_the_later_ones (constants: identifier list): identifier list =
       match constants with
       | first :: later when not (equal_identifier first name) -> this_constant_and_the_later_ones later
       | _ -> constants
     in
     let not_yet_defined: identifier list = this_constant_and_the_later_ones module_constants in
     let expr' = TypeCheckExpr.check (TypeCheckExpr.make_constant_value_ctx module_name env place not_yet_defined) expr ty in
     let _ = validate_constant_expression place expr' in
     (TConstant (decl_id, vis, name, ty, expr', doc), supply)
  | LRecord (decl_id, vis, name, typarams, universe, slots, doc) ->
     (TRecord (decl_id, vis, name, typarams, universe, slots, doc), supply)
  | LUnion (decl_id, vis, name, typarams, universe, cases, doc) ->
     (TUnion (decl_id, vis, name, typarams, universe, cases, doc), supply)
  | LFunction (decl_id, vis, name, typarams, params, rt, body, doc, pragmas) ->
     let lexenv = lexenv_from_params place params in
     let ctx = StmtCtx (module_name, env, rm, typarams, rt, place) in
     let _ =
       List.map (fun (ValueParameter (name, _)) -> check_var_doesnt_collide_with_decl ctx name) params
     in
     (* Whether the function is foreign was decided when it was registered
        (ExtractionPass), from its pragmas, and the environment keeps it. *)
     let { impl; _ }: function_decl = get_function env decl_id in
     (match impl with
      | Foreign (s, signature) ->
         (match kind with
          | UnsafeModule ->
             (TForeignFunction (decl_id, vis, name, params, rt, s, signature, doc), supply)
          | SafeModule ->
             declaration_error place ForeignInSafeModule)
      | ForeignGeneric ->
         declaration_error place ForeignTypeParameters
      | BodyNotAttached | Body _ ->
         (* One Foreign_Import pragma made the function foreign, so here
            [ForeignImportPragma _] stands alone only with type parameters,
            which ForeignGeneric took. *)
         (match pragmas with
          | [ForeignExportPragma _] ->
             (* C code calls an exported function, and C code is outside what
                a safe module guarantees, as a foreign function is (#161). *)
             (match kind with
              | UnsafeModule ->
                 let (body', _, supply) = augment_stmt ctx lexenv supply body in
                 (TFunction (decl_id, vis, name, typarams, params, rt, body', doc), supply)
              | SafeModule ->
                 declaration_error place ExportInSafeModule)
          | [] ->
             let (body', _, supply) = augment_stmt ctx lexenv supply body in
             (TFunction (decl_id, vis, name, typarams, params, rt, body', doc), supply)
          | [ForeignImportPragma _] | [UnsafeModulePragma] | _ :: _ :: _ ->
             declaration_error place FunInvalidPragmas))
  | LTypeclass (decl_id, vis, name, typaram, methods, doc) ->
     (TTypeClass (decl_id, vis, name, typaram, List.map (augment_method_decl env rm typaram) methods, doc), supply)
  | LInstance (decl_id, vis, name, typarams, arg, methods, doc) ->
     (* TODO: the universe of the type parameter matches the universe of the type argument *)
     (* TODO: Check the methods in the instance match the methods in the class *)
     let (methods, supply) = Supply.map (augment_method_def module_name env rm place typarams) supply methods in
     (TInstance (decl_id, vis, name, typarams, arg, methods, doc), supply)

and lexenv_from_params (place: place) (params: value_parameter list): lexenv =
  match params with
  | (ValueParameter (n, t))::rest ->
     push_var place (lexenv_from_params place rest) n t VarParam
  | [] ->
     empty_lexenv

and augment_method_decl _ _ _ (LMethodDecl (decl_id, name, params, rt, _)) =
  TypedMethodDecl (decl_id, name, params, rt)

(* [place] is the span of the instance. *)
and augment_method_def module_name menv rm place instance_typarams supply (LMethodDef (ins_meth_id, name, params, rt, _, body)) =
  (* The body can name the type parameters of the instance and those of the
     method itself, as the signature can (ExtractionPass). The environment
     keeps the method's own. *)
  let (InsMethRec { typarams = method_typarams; _ }) = get_instance_method menv ins_meth_id in
  let typarams = merge_typarams place instance_typarams method_typarams in
  let lexenv = lexenv_from_params place params in
  let ctx = StmtCtx (module_name, menv, rm, typarams, rt, place) in
  let _ =
    List.map (fun (ValueParameter (name, _)) -> check_var_doesnt_collide_with_decl ctx name) params
  in
  let (body', _, supply) = augment_stmt ctx lexenv supply body in
  (TypedMethodDef (ins_meth_id, name, params, rt, body'), supply)

let augment_module menv (supply: Supply.t) (LinkedModule { name; decls; kind; _ }): (typed_module * Supply.t) Outcome.t =
  (* In the order the body defines them. The span of a constant is that of its
     definition in the body, also for one the interface declares
     (CombiningPass), and a span is plain data: OCaml's order puts the earlier
     one of a file first. The declarations are not in that order: those of the
     interface come before the private ones. *)
  let module_constants: identifier list =
    List.map snd
      (List.stable_sort (fun (span, _) (span', _) -> compare span span')
         (List.filter_map
            (fun (span, decl) ->
              match decl with
              | LConstant (_, _, name, _, _, _) -> Some (span, name)
              | LRecord _ | LUnion _ | LFunction _ | LTypeclass _ | LInstance _ -> None)
            decls))
  in
  Outcome.map (fun (supply, decls') -> (TypedModule (name, decls'), supply))
    (Outcome.traverse_with_context
       (fun (supply, (span, decl)) -> Outcome.catch (fun () -> let (decl, supply) = augment_decl name kind menv module_constants supply (AtSpan span) decl in (supply, decl)))
       supply
       decls)
