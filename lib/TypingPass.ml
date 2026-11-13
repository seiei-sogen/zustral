(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
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

type stmt_ctx = StmtCtx of module_name * env * region_map * typarams * ty

let update_rm (StmtCtx (mn, menv, _, typarams, rt)) (rm: region_map): stmt_ctx =
  StmtCtx (mn, menv, rm, typarams, rt)

(* Since the extraction pass has already happened, we can simplify the call to
   `parse_type` by passing an empty list of local type signatures. *)
let parse_typespec (env: env) (rm: region_map) (typarams: typarams) (ty: qtypespec): ty =
  parse_type place_from_adorn env [] rm typarams ty

let infer_expr (module_name: module_name) (env: env) (rm: region_map) (typarams: typarams) (lexenv: lexenv) (expr: aexpr): texpr =
  let ctx: TypeCheckExpr.expr_ctx = TypeCheckExpr.make_ctx module_name env rm typarams lexenv in
  TypeCheckExpr.infer ctx expr

let check_expr (module_name: module_name) (env: env) (rm: region_map) (typarams: typarams) (lexenv: lexenv) (expr: aexpr) (expected: ty): texpr =
  let ctx: TypeCheckExpr.expr_ctx = TypeCheckExpr.make_ctx module_name env rm typarams lexenv in
  TypeCheckExpr.check ctx expr expected

(** Copied from TypeCheckExpr *)
let check_annotated_expr (module_name: module_name) (env: env) (rm: region_map) (typarams: typarams) (lexenv: lexenv) (expr: aexpr) (annotation: ty): texpr =
  let ctx: TypeCheckExpr.expr_ctx = TypeCheckExpr.make_ctx module_name env rm typarams lexenv in
  TypeCheckExpr.check_annotated ctx expr annotation

let get_record_definition (env: env) (name: qident) (ty: ty): (module_name * type_vis * typarams * typed_slot list) =
  match get_decl_by_name env (qident_to_sident name) with
  | (Some (Record { mod_id; vis; typarams; slots; _ })) ->
     let mod_name: module_name = module_name_from_id env mod_id in
     (mod_name, vis, typarams, slots)
  | Some _ ->
     type_error (PathNotRecord ty)
  | None ->
     generic_error (NoRecordWithName (original_name name))

let is_boolean = function
  | Boolean -> true
  | Unit | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _ | ReadRef _
  | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
  | MonoTy _ -> false

let is_compatible_with_index_type = function
  | TIntConstant _ ->
     true
  | e ->
     (get_type e) = index_type

type case_mode =
  | NormalCaseMode
  | ReadRefCaseMode of ref_region
  | WriteRefCaseMode of ref_region

let check_var_doesnt_collide_with_decl (ctx: stmt_ctx) (name: identifier) =
  let (StmtCtx (module_name, env, _, _, _)) = ctx in
  let sname: sident = make_sident module_name name in
  match get_decl_by_name env sname with
  | Some _ ->
     generic_error (VarCollidesWithDecl name)
  | None ->
     ()

(* The span of `stmt` if it assigns a temporary, which is what
   `LiftControlPass` turns every assignment into. `DesugarBorrows` wraps such a
   statement in the anonymous borrows of its lvalue. *)
let rec lifted_assignment_span (stmt: astmt): span option =
  match stmt with
  | AAssign (span, _, Temporary _) | AAssignVar (span, _, Temporary _) ->
     Some span
  | ABorrow { body; _ } ->
     lifted_assignment_span body
  | _ ->
     None

(* The table with the temporary on the right-hand side of an assignment,
   declared with the type of the lvalue. The statement that computes the
   temporary is checked after the assignment (see `augment_block`), against
   that type. *)
let declare_assigned_temporary (lexenv: lexenv) (rvalue: aexpr) (ty: ty): lexenv =
  match rvalue with
  | Temporary name ->
     push_var lexenv name ty (VarLocal Mutable)
  | _ ->
     lexenv

(* The supply numbers the regions of the borrow statements, in the order the
   statements are checked. That order is not the order of the source: the
   `else` branch of an `if` is checked before the `then` branch, and an
   assignment before the statement that computes its value. The numbers are in
   error messages, and which of two errors is reported depends on the order
   too, so it stays.

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
  | ABorrow { span; original; rename; region; body; mode; } ->
     augment_borrow ctx lexenv supply span original rename region body mode
  | ABlock (span, f, r) ->
     augment_block ctx lexenv supply span f r
  | ADiscarding (span, e) ->
     augment_discarding ctx lexenv supply span e
  | AReturn (span, e) ->
     augment_return ctx lexenv supply span e

and augment_let (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (mut: mutability) (name: identifier) (ty: qtypespec) (body: astmt): tstmt * lexenv * Supply.t =
  let (StmtCtx (_, env, rm, typarams, _)) = ctx in
  adorn_error_with_span span
    (fun _ ->
      let _ = check_var_doesnt_collide_with_decl ctx name in
      let ty = parse_typespec env rm typarams ty in
      let lexenv = push_var lexenv name ty (VarLocal mut) in
      let (body', lexenv, supply) = augment_stmt ctx lexenv supply body in
      (TLet (span, mut, name, ty, body'), remove_var lexenv name, supply))

and augment_let_tmp (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (name: identifier) (value: aexpr): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _)) = ctx in
  (match get_var lexenv name with
   | Some (ty, _) ->
      (* The temporary holds the right-hand side of an assignment, which
         has declared it with the type of its lvalue. That type is the
         expected type of the right-hand side, as the annotation of a
         `let` is for its value. *)
      let value = check_expr module_name env rm typarams lexenv value ty in
      (TLetTmp (name, ty, value), lexenv, supply)
   | None ->
      let value = infer_expr module_name env rm typarams lexenv value in
      let ty = get_type value in
      let lexenv = push_var lexenv name ty (VarLocal Mutable) in
      (TLetTmp (name, ty, value), lexenv, supply))

and augment_assign_tmp (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (name: identifier) (value: aexpr): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _)) = ctx in
  let value = infer_expr module_name env rm typarams lexenv value in
  begin
    match get_var lexenv name with
    | Some (ty, _) ->
       let _ = match_type (env, module_name) ty (get_type value) in
       (TAssignTmp (name, value), lexenv, supply)
    | None ->
       internal_err "Unknown temporary."
  end

and augment_destructure (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (mut: mutability) (bindings: qbinding list) (value: aexpr) (body: astmt): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _)) = ctx in
  adorn_error_with_span span
    (fun _ ->
      let _ =
        List.map (fun (Stages.Ast.QBinding { rename; _ }) -> check_var_doesnt_collide_with_decl ctx rename) bindings
      in
      let value' = infer_expr module_name env rm typarams lexenv value in
      (* Check: the value must be a public record type *)
      let rec_ty = get_type value' in
      (match rec_ty with
       | (NamedType (name, _, u)) ->
          let (source_module, vis, record_typarams, slots) = get_record_definition env name rec_ty in
          let orig_type = NamedType (
                              make_qident (source_module, original_name name, original_name name),
                              List.map (fun tp -> TyVar (typaram_to_tyvar tp)) (typarams_as_list record_typarams),
                              u
                            )
          in
          let typebindings = match_type (env, module_name) orig_type rec_ty in
          if (vis = TypeVisPublic) || (module_name = source_module) then
            (* Find the set of slot names and the set of binding names, and compare them *)
            let binding_names = List.map (fun (Stages.Ast.QBinding { name; _}) -> name) bindings
            and slot_names = List.map (fun (TypedSlot (n, _)) -> n) slots in
            if ident_set_eq binding_names slot_names then
              let bindings': (identifier * qtypespec * ty * identifier) list =
                group_bindings_slots bindings slots
              in
              let bindings'': (identifier * ty * ty * identifier) list
                = List.map (fun (n, ty, actual, rename) -> (n, parse_typespec env rm typarams ty, replace_variables typebindings actual, rename)) bindings'
              in
              let newvars: (identifier * ty * var_source) list =
                List.map (fun (_, ty, actual, rename) ->
                    let _ = match_type (env, module_name) ty actual in
                    (rename, ty, VarLocal mut))
                  bindings''
              in
              let lexenv = push_vars lexenv newvars in
              let (body', lexenv, supply) = augment_stmt ctx lexenv supply body in
              (TDestructure (
                  span,
                  mut,
                  List.map (fun (name, _, ty, rename) -> TypedBinding { name; ty; rename; }) bindings'',
                  value',
                  body'
                ), remove_vars lexenv (List.map (fun (n, _, _) -> n) newvars), supply)
            else
              type_error (DestructureWrongSlots rec_ty)
          else
            type_error (DestructureNotPublic rec_ty)
       | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _
       | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _
       | Pointer _ | FnPtr _ | MonoTy _ ->
          type_error (DestructureNonRecord rec_ty)))

and augment_assign (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (lvalue: aexpr) (rvalue: aexpr): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _)) = ctx in
  adorn_error_with_span span
    (fun _ ->
      (* Augment the lvalue. *)
      let lvalue: texpr = infer_expr module_name env rm typarams lexenv lvalue in
      (* The type of the lvalue must be a read reference. *)
      let lvalue_ty: ty = get_type lvalue in
      let pointed_ty: ty = begin
          match lvalue_ty with
          | WriteRef (ty, _) ->
             ty
          | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
          | RegionTy _ | ReadRef _ | Span _ | SpanMut _ | TyVar _ | Address _
          | Pointer _ | FnPtr _ | MonoTy _ ->
             generic_error (AssignToNonMutableReference lvalue_ty)
        end
      in
      (* Augment the rvalue. *)
      let lexenv = declare_assigned_temporary lexenv rvalue pointed_ty in
      let rvalue: texpr = infer_expr module_name env rm typarams lexenv rvalue in
      (* The pointed to type and the type of the rvalue must match. *)
      let _ = match_type_with_value (env, module_name) pointed_ty rvalue in
      (* The pointed-to type must be in the free universe. *)
      let _ =
        if (type_universe pointed_ty) = FreeUniverse then
          ()
        else
          linearity_error (AssignToNonFreePath pointed_ty)
      in
      (* Construct the assignment. *)
      (TAssign (span, lvalue, rvalue), lexenv, supply))

and augment_assign_var (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (name: qident) (rvalue: aexpr): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _)) = ctx in
  adorn_error_with_span span
    (fun _ ->
      (* Get the type of the lvalue. *)
      let lvalue_ty: ty = begin
          let name: identifier = original_name name in
        match get_var lexenv name with
        | (Some (orig_ty, src)) -> begin
            let _ =
              match src with
              | VarLocal Mutable ->
                 ()
              | VarLocal Immutable ->
                 generic_error (CantAssignToImmutableVar name)
              | VarConstant ->
                 generic_error CannotAssignToConstant
              | VarParam ->
                 generic_error CannotAssignToParameter
            in
            orig_ty
          end
        | None ->
           generic_error (UnknownName { kind = VariableName; name })
        end
      in
      (* Augment the rvalue. *)
      let lexenv = declare_assigned_temporary lexenv rvalue lvalue_ty in
      let rvalue: texpr = infer_expr module_name env rm typarams lexenv rvalue in
      (* Types must match. *)
      let _ = match_type_with_value (env, module_name) lvalue_ty rvalue in
      (* Construct the assignment. *)
      (TAssignVar (span, name, rvalue), lexenv, supply))

and augment_initial_assign (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (name: qident) (ty: qtypespec) (rvalue: aexpr): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _)) = ctx in
  (* The type of the `let` annotates its value. *)
  let ty = parse_typespec env rm typarams ty in
  let rvalue: texpr = check_annotated_expr module_name env rm typarams lexenv rvalue ty in
  let _ = match_type_with_value (env, module_name) ty rvalue in
  (TInitialAssign (name, rvalue), lexenv, supply)

and augment_if (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (c: aexpr) (t: astmt) (f: astmt): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _)) = ctx in
  adorn_error_with_span span
    (fun _ ->
      let c' = infer_expr module_name env rm typarams lexenv c in
      if is_boolean (get_type c') then
        let (f, lexenv, supply) = augment_stmt ctx lexenv supply f in
        let (t, lexenv, supply) = augment_stmt ctx lexenv supply t in
        (TIf (span, c', t, f), lexenv, supply)
      else
        type_error (ConditionNotBoolean { condition = IfStatementCondition; ty = get_type c' }))

and augment_if_without_else (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (c: aexpr) (t: astmt): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _)) = ctx in
  adorn_error_with_span span
    (fun _ ->
      let c = infer_expr module_name env rm typarams lexenv c in
      if is_boolean (get_type c) then
        let (t, lexenv, supply) = augment_stmt ctx lexenv supply t in
        (TIf (span, c, t, TSkip span), lexenv, supply)
      else
        type_error (ConditionNotBoolean { condition = IfStatementCondition; ty = get_type c }))

and augment_while (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (c: aexpr) (body: astmt): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _)) = ctx in
  adorn_error_with_span span
    (fun _ ->
      let c' = infer_expr module_name env rm typarams lexenv c in
      if is_boolean (get_type c') then
        let (body, lexenv, supply) = augment_stmt ctx lexenv supply body in
        (TWhile (span, c', body), lexenv, supply)
      else
        type_error (ConditionNotBoolean { condition = WhileStatementCondition; ty = get_type c' }))

and augment_for (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (name: identifier) (initial: aexpr) (final: aexpr) (body: astmt): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _)) = ctx in
  adorn_error_with_span span
    (fun _ ->
      let i' = infer_expr module_name env rm typarams lexenv initial
      and f' = infer_expr module_name env rm typarams lexenv final in
      if is_compatible_with_index_type i' then
        if is_compatible_with_index_type f' then
          let lexenv = push_var lexenv name index_type (VarLocal Immutable) in
          let (b', lexenv, supply) = augment_stmt ctx lexenv supply body in
          (TFor (span, name, i', f', b'), remove_var lexenv name, supply)
        else
          type_error (ForBoundsNonIntegral { bound = FinalBound; ty = get_type f' })
      else
        type_error (ForBoundsNonIntegral { bound = InitialBound; ty = get_type i' }))

and augment_borrow (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (original: identifier) (rename: identifier) (region: identifier) (body: astmt) (mode: borrow_stmt_kind): tstmt * lexenv * Supply.t =
  let (StmtCtx (_, _, rm, _, _)) = ctx in
  adorn_error_with_span span
    (fun _ ->
      let _ = check_var_doesnt_collide_with_decl ctx rename in
      (match get_var lexenv original with
       | (Some (orig_ty, src)) ->
          (* Check we can borrow the variable. *)
          let _ =
            match (src, mode) with
            | (_, Read) ->
               (* Anything can be borrowed immutably. *)
               ()
            | (VarLocal Immutable, Write) ->
               (* Immutable variables cannot be borrowed mutably. *)
               type_error (CannotBorrowImmutableVarMutably original)
            | (VarParam, Write) ->
               (* Parameters cannot be borrowed mutably, unless they're mutable references. *)
               let is_mut = begin
                 match orig_ty with
                 | WriteRef _ -> true
                 | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat
                 | NamedType _ | RegionTy _ | ReadRef _ | Span _ | SpanMut _
                 | TyVar _ | Address _ | Pointer _ | FnPtr _ | MonoTy _ -> false
               end
               in
               if is_mut then
                 ()
               else
                 type_error (CannotBorrowParamMutably { name = original; ty = orig_ty })
            | _ ->
               (* And anything else is fine. *)
               ()
          in
          let (region_obj, supply) = fresh_region supply in
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
                    | TyVar _ | Address _ | Pointer _ | FnPtr _ | MonoTy _ ->
                       generic_error (ReborrowOfNonMutableReference orig_ty)
                  end
                in
                WriteRef (pointed_ty, RefRegion region_obj))
          in
          let lexenv = push_var lexenv rename refty (VarLocal Immutable) in
          let rm' = add_region rm region region_obj in
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
              mode=mode
            }, remove_var lexenv rename, supply)
       | None ->
          generic_error (UnknownName { kind = VariableName; name = original })))

and augment_block (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (f: astmt) (r: astmt): tstmt * lexenv * Supply.t =
  (match lifted_assignment_span r with
   | Some assign_span ->
      (* `f` computes the temporary that `r` assigns. `r` is checked
         first because it gives the temporary its type, and the type of
         a path is only known inside the borrows around `r`. An error
         in `f` has no span of its own. *)
      let (b, lexenv, supply) = augment_stmt ctx lexenv supply r in
      let (a, lexenv, supply) = adorn_error_with_span assign_span (fun _ -> augment_stmt ctx lexenv supply f) in
      (TBlock (span, a, b), lexenv, supply)
   | None ->
      let (a, lexenv, supply) = augment_stmt ctx lexenv supply f in
      let (b, lexenv, supply) = augment_stmt ctx lexenv supply r in
      (TBlock (span, a, b), lexenv, supply))

and augment_discarding (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (e: aexpr): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _)) = ctx in
  adorn_error_with_span span
    (fun _ ->
      let e' = infer_expr module_name env rm typarams lexenv e in
      let u = type_universe (get_type e') in
      if ((u = LinearUniverse) || (u = TypeUniverse)) then
        type_error (DiscardLinear u)
      else
        (TDiscarding (span, e'), lexenv, supply))

and augment_return (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (e: aexpr): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, rt)) = ctx in
  adorn_error_with_span span
    (fun _ ->
      let e' = check_expr module_name env rm typarams lexenv e rt in
      (TReturn (span, e'), lexenv, supply))

and augment_case (ctx: stmt_ctx) (lexenv: lexenv) (supply: Supply.t) (span: span) (expr: aexpr) (whens: abstract_when list): tstmt * lexenv * Supply.t =
  let (StmtCtx (module_name, env, rm, typarams, _)) = ctx in
  (* Type checking a case statement:

     1. Ensure the value is of a union type (or reference to a union).
     2. Ensure the union type is public or it is defined in this module.
     3. Ensure the set of case names in the case statement equals the set of cases in the union definition.
     4. Iterate over the cases, and ensure the bindings are correct.
   *)
  adorn_error_with_span span
    (fun _ ->
      let expr' = infer_expr module_name env rm typarams lexenv expr in
      let ty = get_type expr' in
      let ty, mode =
        (match ty with
         | NamedType _ ->
            (ty, NormalCaseMode)
         | ReadRef (ty, r) ->
            (ty, ReadRefCaseMode r)
         | WriteRef (ty, r) ->
            (ty, WriteRefCaseMode r)
         | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _ | Span _
         | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _ | MonoTy _ ->
            type_error (CaseNotUnionOrReference ty))
      in
      let case_ref: case_ref =
        (match mode with
         | NormalCaseMode -> CasePlain
         | ReadRefCaseMode _ -> CaseRef
         | WriteRefCaseMode _ -> CaseRef)
      in
      let (union_ty, cases) = get_union_type_definition module_name env ty in
      let typebindings = match_type (env, module_name) union_ty ty in
      let case_names = List.map (fun (TypedCase (n, _)) -> n) (List.map union_case_to_typed_case cases) in
      let when_names = List.map (fun (AbstractWhen (n, _, _)) -> n) whens in
      if ident_set_eq case_names when_names then
        (* Group the cases and whens *)
        let whens' = group_cases_whens cases whens in
        let (whens'', lexenv, supply) = augment_whens ctx lexenv supply typebindings whens' mode in
        (TCase (span, expr', whens'', case_ref), lexenv, supply)
      else
        type_error CaseNonExhaustive)

and get_union_type_definition (importing_module: module_name) (env: env) (ty: ty): (ty * union_case_decl list) =
  let name: qident = (match ty with
                      | NamedType (n, _, _) ->
                         n
                      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat
                      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _
                      | TyVar _ | Address _ | Pointer _ | FnPtr _ | MonoTy _ ->
                         type_error (CaseNonUnion ty)) in
  match get_decl_by_name env (qident_to_sident name) with
  | Some (Union { id; vis; name; mod_id; typarams; universe; _ }) ->
     let module_name = module_name_from_id env mod_id in
     if (vis = TypeVisPublic) || (importing_module = module_name) then
       let n = make_qident (module_name, name, name)
       and args = List.map (fun tp -> TyVar (typaram_to_tyvar tp)) (typarams_as_list typarams)
       in
       let ty = NamedType (n, args, universe)
       and cases = get_union_cases env id in
       (ty, cases)
     else
       type_error CaseNonPublic
  | _ ->
     type_error (CaseNonUnion ty)

and group_cases_whens (cases: union_case_decl list) (whens: abstract_when list): (typed_case * abstract_when) list =
  List.map (fun (TypedCase (n, s)) -> (TypedCase (n, s), List.find (fun (AbstractWhen (n', _, _)) -> n = n') whens))
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
  let (StmtCtx (_, menv, rm, typarams, _)) = ctx in
  let (AbstractWhen (name, bindings, body)) = w
  and (TypedCase (_, slots)) = c in
  (* Check the set of binding names is the same as the set of slots *)
  let binding_names = List.map (fun (Stages.Ast.QBinding { name; _ }) -> name) bindings
  and slot_names = List.map (fun (TypedSlot (n, _)) -> n) slots in
  if ident_set_eq binding_names slot_names then
    (* Check the type of each binding matches the type of the slot *)
    let bindings' = group_bindings_slots bindings slots in
    let bindings'' = List.map (fun (n, ty, actual, rename) -> (n, parse_typespec menv rm typarams ty, replace_variables typebindings actual, rename)) bindings' in
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
                        type_error (SlotWrongType {
                            name = rename;
                            expected = decl_ty;
                            actual = user_ty;
                          }))
                    bindings'' in
    let lexenv = push_vars lexenv newvars in
    let (body', lexenv, supply) = augment_stmt ctx lexenv supply body in
    (TypedWhen (name, List.map (fun (name, _, ty, rename) -> TypedBinding { name; ty; rename}) bindings'', body'), remove_vars lexenv (List.map (fun (n, _, _) -> n) newvars), supply)
  else
    type_error (CaseWrongSlots { binding_names; slot_names })

let rec validate_constant_expression (expr: texpr): unit =
  if is_constant expr then
    ()
  else
    type_error ExpressionNotConstant

and is_constant = function
  | TNilConstant ->
     true
  | TBoolConstant _ ->
     true
  | TIntConstant _ ->
     true
  | TFloatConstant _ ->
     true
  | TStringConstant _ ->
     true
  | TConstVar _ ->
     true
  | TParamVar _ ->
     false
  | TLocalVar _ ->
     false
  | TTemporary _ ->
     false
  | TFunVar _ ->
     true
  | TFuncall _ ->
     false
  | TMethodCall _ ->
     false
  | TVarMethodCall _ ->
     false
  | TFptrCall _ ->
     false
  | TCast _ | TImplicitCast _ ->
     true
  | TComparison (_, lhs, rhs) ->
     (is_constant lhs) && (is_constant rhs)
  | TConjunction (lhs, rhs) ->
     (is_constant lhs) && (is_constant rhs)
  | TDisjunction (lhs, rhs) ->
     (is_constant lhs) && (is_constant rhs)
  | TNegation e ->
     is_constant e
  | TIfExpression (c, t, f) ->
     (is_constant c) && (is_constant t) && (is_constant f)
  | TRecordConstructor (_, values) ->
     List.for_all (fun (_, v) -> is_constant v) values
  | TUnionConstructor (_, _, values) ->
     List.for_all (fun (_, v) -> is_constant v) values
  | TEmbed _ ->
     true
  | TDeref _ ->
     false
  | TSizeOf _ ->
     true
  | TSlotAccessor _ ->
     true
  | TPointerSlotAccessor _ ->
     true
  | TArrayIndex (_, idx, _) ->
     is_constant idx
  | TSpanIndex (_, idx, _) ->
     is_constant idx

let rec augment_decl (module_name: module_name) (kind: module_kind) (env: env) (supply: Supply.t) (decl: linked_definition): typed_decl * Supply.t =
  let rm = empty_region_map in
  match decl with
  | LConstant (decl_id, vis, name, ty, expr, doc) ->
     let expr' = check_expr module_name env rm empty_typarams empty_lexenv expr ty in
     let _ = validate_constant_expression expr' in
     (TConstant (decl_id, vis, name, ty, expr', doc), supply)
  | LRecord (decl_id, vis, name, typarams, universe, slots, doc) ->
     (TRecord (decl_id, vis, name, typarams, universe, slots, doc), supply)
  | LUnion (decl_id, vis, name, typarams, universe, cases, doc) ->
     (TUnion (decl_id, vis, name, typarams, universe, cases, doc), supply)
  | LFunction (decl_id, vis, name, typarams, params, rt, body, doc, pragmas) ->
     let lexenv = lexenv_from_params params in
     let ctx = StmtCtx (module_name, env, rm, typarams, rt) in
     let _ =
       List.map (fun (ValueParameter (name, _)) -> check_var_doesnt_collide_with_decl ctx name) params
     in
     (match pragmas with
      | [ForeignImportPragma s] ->
         if typarams_size typarams = 0 then
           if kind = UnsafeModule then
             (TForeignFunction (decl_id, vis, name, params, rt, s, doc), supply)
           else
             declaration_error ForeignInSafeModule
         else
           declaration_error ForeignTypeParameters
      | [ForeignExportPragma _] ->
         let (body', _, supply) = augment_stmt ctx lexenv supply body in
         (TFunction (decl_id, vis, name, typarams, params, rt, body', doc), supply)
      | [] ->
         let (body', _, supply) = augment_stmt ctx lexenv supply body in
         (TFunction (decl_id, vis, name, typarams, params, rt, body', doc), supply)
      | _ ->
         declaration_error FunInvalidPragmas)
  | LTypeclass (decl_id, vis, name, typaram, methods, doc) ->
     (TTypeClass (decl_id, vis, name, typaram, List.map (augment_method_decl env rm typaram) methods, doc), supply)
  | LInstance (decl_id, vis, name, typarams, arg, methods, doc) ->
     (* TODO: the universe of the type parameter matches the universe of the type argument *)
     (* TODO: Check the methods in the instance match the methods in the class *)
     let (methods, supply) = Supply.map (augment_method_def module_name env rm typarams) supply methods in
     (TInstance (decl_id, vis, name, typarams, arg, methods, doc), supply)

and lexenv_from_params (params: value_parameter list): lexenv =
  match params with
  | (ValueParameter (n, t))::rest ->
     push_var (lexenv_from_params rest) n t VarParam
  | [] ->
     empty_lexenv

and augment_method_decl _ _ _ (LMethodDecl (decl_id, name, params, rt, _)) =
  TypedMethodDecl (decl_id, name, params, rt)

and augment_method_def module_name menv rm typarams supply (LMethodDef (ins_meth_id, name, params, rt, _, body)) =
  let lexenv = lexenv_from_params params in
  let ctx = StmtCtx (module_name, menv, rm, typarams, rt) in
  let _ =
    List.map (fun (ValueParameter (name, _)) -> check_var_doesnt_collide_with_decl ctx name) params
  in
  let (body', _, supply) = augment_stmt ctx lexenv supply body in
  (TypedMethodDef (ins_meth_id, name, params, rt, body'), supply)

let augment_module menv (supply: Supply.t) (LinkedModule { name; decls; kind; _ }): (typed_module * Supply.t) Outcome.t =
  Outcome.map (fun (supply, decls') -> (TypedModule (name, decls'), supply))
    (Outcome.traverse_with_context
       (fun (supply, decl) -> Outcome.catch (fun () -> let (decl, supply) = augment_decl name kind menv supply decl in (supply, decl)))
       supply
       decls)
