(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Id
open Identifier
open IdentifierSet
open Type
open TypeBindings
open TypeParameter
open TypeParameters
open SignatureTypeAtUse
open Stages.Tast
open TastUtil
open Env
open EnvTypes
open Error

type ctx = env * module_name * place

(* The instances that a constraint of a type parameter is looked up among. *)
type instance_scope =
  | InstancesVisibleFrom of module_name
  (* Where a type is checked: the instances of the module, and those it
     imports. *)
  | InstancesOfEveryModule
  (* Where a generic function is instantiated. The type checker checked the
     constraints where the function was called, in a module that can see
     instances that the module of the function cannot (#128). *)

(* What the matcher works with: a `ctx` that says the scope. *)
type scoped_ctx = env * instance_scope * place

let ctx_env (env, _, _) = env

let ctx_scope ((_, scope, _): scoped_ctx): instance_scope = scope

let ctx_place (_, _, place) = place

open Outcome.Syntax

(* List.map2 for a function that returns its failure. It does not call the
   function after the first failure. Every caller passes two lists of one
   length: the type arguments of one declaration, or the types of two function
   pointer types whose numbers of parameters the matcher compared first (issue
   #85). List.fold_left2 raises Invalid_argument otherwise. *)
let map2_result (f: 'a -> 'b -> ('c, 'e) result) (l1: 'a list) (l2: 'b list): ('c list, 'e) result =
  let* reversed =
    List.fold_left2
      (fun (so_far: ('c list, 'e) result) (a1: 'a) (a2: 'b) ->
        let* rs = so_far in
        let* r = f a1 a2 in
        Ok (r :: rs))
      (Ok []) l1 l2
  in
  Ok (List.rev reversed)

(* A function named try_ returns a failed match as a value, the error that
   reports it. Two callers go on after a failure. The instance lookup tries
   the next instance, and a cast reports an error of its own. Every other
   caller uses the raising functions after this group. *)
let rec try_match_type (ctx: scoped_ctx) (a: ty) (b: ty): (type_bindings, error) result =
  match a with
  | Unit ->
     (match b with
      | Unit ->
         Ok empty_bindings
      | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
      | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | Boolean ->
     (match b with
      | Boolean ->
         Ok empty_bindings
      | Unit | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
      | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | Integer (s, w) ->
     (match b with
      | Integer (s', w') ->
         if (s = s') && (w = w') then
           Ok empty_bindings
         else
           Error (Type (TypeMismatch { expected = a; actual = b }))
      | Unit | Boolean | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
      | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | SingleFloat ->
     (match b with
      | SingleFloat ->
         Ok empty_bindings
      | Unit | Boolean | Integer _ | DoubleFloat | NamedType _ | RegionTy _ | ReadRef _
      | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | DoubleFloat ->
     (match b with
      | DoubleFloat ->
         Ok empty_bindings
      | Unit | Boolean | Integer _ | SingleFloat | NamedType _ | RegionTy _ | ReadRef _
      | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | NamedType (n, _, args, _) ->
     (match b with
      | NamedType (n', _, args', _) ->
         (* We ignore the universe, since the type system is nominal. The
            name the program wrote is not compared: an import can rename a
            type (#50). *)
         if equal_qident n n' then
           try_match_type_list ctx args args'
         else
           Error (Type (TypeMismatch { expected = a; actual = b }))
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _ | ReadRef _
      | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | RegionTy r ->
     (match b with
      | RegionTy r' ->
         if r = r' then
           Ok empty_bindings
         else
           Error (Type (TypeMismatch { expected = a; actual = b }))
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | ReadRef (t, r) ->
     (match b with
      | ReadRef (t', r') ->
         let* bindings = try_match_type ctx t t' in
         let* bindings' = try_match_type ctx (ty_of_ref_region r) (ty_of_ref_region r') in
         try_merge_bindings bindings bindings'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | WriteRef (t, r) ->
     (match b with
      | WriteRef (t', r') ->
         let* bindings = try_match_type ctx t t' in
         let* bindings' = try_match_type ctx (ty_of_ref_region r) (ty_of_ref_region r') in
         try_merge_bindings bindings bindings'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | Span (t, r) ->
     (match b with
      | Span (t', r') ->
         let* bindings = try_match_type ctx t t' in
         let* bindings' = try_match_type ctx (ty_of_ref_region r) (ty_of_ref_region r') in
         try_merge_bindings bindings bindings'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | SpanMut _ | TyVar _ | Address _
      | Pointer _ | FnPtr _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | SpanMut (t, r) ->
     (match b with
      | SpanMut (t', r') ->
         let* bindings = try_match_type ctx t t' in
         let* bindings' = try_match_type ctx (ty_of_ref_region r) (ty_of_ref_region r') in
         try_merge_bindings bindings bindings'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | TyVar tyvar ->
     try_match_type_var ctx tyvar b
  | Address t ->
     (match b with
      | Address t' ->
         try_match_type ctx t t'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Pointer _
      | FnPtr _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | Pointer t ->
     (match b with
      | Pointer t' ->
         try_match_type ctx t t'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _
      | FnPtr _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | FnPtr (args, rt) ->
     (match b with
      | FnPtr (args', rt') when List.length args = List.length args' ->
         try_match_type_list ctx (rt :: args) (rt' :: args')
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _
      | Pointer _ | FnPtr _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))

and try_match_type_var (ctx: scoped_ctx) (tv: type_var) (ty: ty): (type_bindings, error) result =
  let (TypeVariable (_, _, _, constraints, matching)) = tv in
  match matching with
  | StandsForItself ->
     (* A type parameter of the declaration being checked. A binding would
        accept a body that is right for one type argument only (#151), and a
        check of its constraints or its universe would say that the other
        type could be the type argument (#156). *)
     if equal_ty (TyVar tv) ty then
       Ok empty_bindings
     else
       Error (Type (TypeMismatch { expected = TyVar tv; actual = ty }))
  | BoundAtUse ->
     (match ty with
      | TyVar (TypeVariable (_, _, _, constraints', _)) ->
         (* The argument's type variable must have every constraint of the
            parameter's: its constraints are all that is known of the type it
            stands for (#128, #158). *)
         (match List.find_opt (fun (c: sident) -> not (List.mem c constraints')) constraints with
          | Some typeclass ->
             Error (Type (UnsatisfiedConstraint { tyvar = tv; ty; typeclass }))
          | None ->
             (* The function `f` takes an argument of the type `T`. The body of
                the function `g` calls `f(x)` with `x` of its own type parameter
                `U`: the call binds `T` to `U`. When `g` is `f`, it binds `T` to
                the `T` of the caller. *)
             try_add_binding empty_bindings (tyvar_to_typaram tv) ty)
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
      | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | Address _ | Pointer _ | FnPtr _ ->
         (* Check that the type implements the type variable's constraints, if any. *)
         let* () = check_type_implements_constraints ctx tv ty constraints in
         (* If the constraints are satisfied, add a straightforward binding. *)
         try_add_binding empty_bindings (tyvar_to_typaram tv) ty)

and check_type_implements_constraints (ctx: scoped_ctx) (tv: type_var) (ty: ty) (constraints: sident list): (unit, error) result =
  (* If there are no constraints, do nothing. *)
  if constraints = [] then
    Ok ()
  else
    (* If there are constraints, make them into a set. *)
    let constraints: SIdentSet.t = SIdentSet.of_list constraints in
    (* Try to find an instance for this type for each of the constraints. *)
    List.fold_left
      (fun (checked: (unit, error) result) (typeclass_name: sident) ->
        let* () = checked in
        try_constraint ctx tv ty typeclass_name)
      (Ok ())
      (List.of_seq (SIdentSet.to_seq constraints))

and try_constraint (ctx: scoped_ctx) (tv: type_var) (ty: ty) (typeclass_name: sident): (unit, error) result =
  (* get_decl_by_name raises when the module of the name is not in the
     environment. The miss is returned here, before it can. *)
  match get_module_by_name (ctx_env ctx) (sident_module_name typeclass_name) with
  | None ->
     Error (Declaration (DeclarationErrors.UnknownModule (sident_module_name typeclass_name)))
  | Some _ ->
  let* id = TypeclassNamedByConstraint.typeclass_named_by_constraint (ctx_place ctx) (ctx_env ctx) tv typeclass_name in
  (* Try to find an instance for the given typeclass name that implements this type. *)
  let* instance = try_get_instance (ctx_place ctx) (ctx_env ctx) (ctx_scope ctx) ty id in
  (match instance with
   | Some _ ->
      Ok ()
   | None ->
      Error (Type (UnsatisfiedConstraint { tyvar = tv; ty; typeclass = typeclass_name })))

and try_get_instance (place: place) (env: env) (scope: instance_scope) (dispatch_ty: ty) (typeclass: typeclass_id): ((instance_decl * type_bindings) option, error) result =
  match scope with
  | InstancesOfEveryModule ->
     try_get_instance_among place env scope (all_instances env) dispatch_ty typeclass
  | InstancesVisibleFrom source_module_name ->
     (match get_module_by_name env source_module_name with
      | None ->
         Error (Declaration (DeclarationErrors.UnknownModule source_module_name))
      (* visible_instances looks the module up again, by this ID. It was found
         here, so that lookup does not miss. *)
      | Some (ModRec { id = mod_id; _ }) ->
         try_get_instance_among place env scope (visible_instances place env mod_id) dispatch_ty typeclass)

(* The one of these instances, those of the scope, that matches the dispatch
   type. A constraint of the instance's type parameters is looked up in the
   same scope. *)
and try_get_instance_among (place: place) (env: env) (scope: instance_scope) (instances: instance_decl list) (dispatch_ty: ty) (typeclass: typeclass_id): ((instance_decl * type_bindings) option, error) result =
  let pred (decl: instance_decl): (instance_decl * type_bindings) option =
    match decl with
    | { typeclass_id; argument; _ } ->
       if equal_typeclass_id typeclass_id typeclass then
         (match try_match_type (env, scope, place) (signature_type_at_use argument) dispatch_ty with
          | Ok bindings ->
             Some (decl, bindings)
          | Error _ ->
             (* Does not match, just skip to the next instance. Every failure
                skips it, also a type error from checking a constraint of the
                instance's type parameters: a constraint that names no
                typeclass. *)
             None)
       else
         None
  in
  let filtered: (instance_decl * type_bindings) list =
    (List.filter_map pred instances)
  in
  match filtered with
  | [a] ->
     Ok (Some a)
  | _::_ ->
     (* Where instances are declared, the overlap check of a module keeps two
        of its instances from matching one type, and the orphan rule (#72)
        keeps two modules from both declaring one: the instance is in the
        module of the typeclass or of the type, and a module imports only
        modules before it. *)
     Error (Internal ("More than one instance matches the type " ^ type_string dispatch_ty))
  | [] ->
     Ok None

and try_match_type_list (ctx: scoped_ctx) (tys: ty list) (tys': ty list): (type_bindings, error) result =
  (* Every pair is matched before any bindings are merged, so a pair that does
     not match is reported before two pairs that bind one parameter to
     different types. *)
  let* bs = map2_result (try_match_type ctx) tys tys' in
  List.fold_left
    (fun (merged: (type_bindings, error) result) (bindings: type_bindings) ->
      let* merged = merged in
      try_merge_bindings merged bindings)
    (Ok empty_bindings)
    bs

(* The matchers of the type checker, which checks the types of a module: a
   constraint is looked up among the instances visible from that module. *)
let scoped_to_module ((env, module_name, place): ctx): scoped_ctx =
  (env, InstancesVisibleFrom module_name, place)

let try_match_type (ctx: ctx) (a: ty) (b: ty): (type_bindings, error) result =
  try_match_type (scoped_to_module ctx) a b

let try_match_type_list (ctx: ctx) (tys: ty list) (tys': ty list): (type_bindings, error) result =
  try_match_type_list (scoped_to_module ctx) tys tys'

let match_type (ctx: ctx) (a: ty) (b: ty): type_bindings =
  ok_or_raise (ctx_place ctx) (try_match_type ctx a b)

let get_instance (place: place) (env: env) (source_module_name: module_name) (dispatch_ty: ty) (typeclass: typeclass_id): (instance_decl * type_bindings) option =
  ok_or_raise place (try_get_instance place env (InstancesVisibleFrom source_module_name) dispatch_ty typeclass)

let get_instance_from_every_module (place: place) (env: env) (dispatch_ty: ty) (typeclass: typeclass_id): (instance_decl * type_bindings) option =
  ok_or_raise place (try_get_instance place env InstancesOfEveryModule dispatch_ty typeclass)

let get_method_of_instance (place: place) (env: env) (instance: instance_decl) (method_name: identifier): ins_meth_id =
  let (InsMethRec { id; _ }) = get_instance_method_from_instance_id_and_method_name place env instance.id method_name in
  id

let match_typarams (ctx: ctx) (typarams: typarams) (args: ty list): type_bindings =
  let typarams' = List.map (fun tp -> signature_type_at_use (TyVar (typaram_to_tyvar tp))) (typarams_as_list typarams) in
  ok_or_raise (ctx_place ctx) (try_match_type_list ctx typarams' args)

(* Two types of the body being checked. Their type variables stand for
   themselves, so the match binds nothing: it only says whether the types are
   the same. *)
let expect_same_type (ctx: ctx) (expected: ty) (actual: ty): unit =
  match try_match_type ctx expected actual with
  | Ok _ ->
     ()
  | Error _ ->
     (* The match reports the two parts that differ. The error is about the
        whole types. *)
     type_error (ctx_place ctx) (TypeMismatch { expected; actual })

let expect_value_of_type (ctx: ctx) (expected: ty) (value: texpr): unit =
  (* A literal that the rules for literals accept does not have to have the
     type. *)
  if LiteralTyping.literal_matches (ctx_place ctx) expected value then
    ()
  else
    (* An integer literal does not get past this: the rules for literals
       accept it for every integer type, and no other type is the same as its
       default type. *)
    expect_same_type ctx expected (get_type (ctx_place ctx) value)

let match_type_with_value (ctx: ctx) (ty: ty) (expr: texpr): type_bindings =
  (* Like match_type, but a literal that the rules for literals accept does not
     have to have the type. *)
  if LiteralTyping.literal_matches (ctx_place ctx) ty expr then
    empty_bindings
  else
    (* The place did not type a literal: it has its default type, which is
       what a type variable of `ty` is bound to. *)
    let bindings = match_type ctx ty (get_type (ctx_place ctx) expr) in
    LiteralTyping.check_int_literal_fits_default_type (ctx_place ctx) expr;
    bindings
