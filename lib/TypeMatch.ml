(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
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
open Stages.Tast
open TastUtil
open Env
open EnvTypes
open Error

type ctx = env * module_name * place

let ctx_env (env, _, _) = env

let ctx_mn (_, mn, _) = mn

let ctx_place (_, _, place) = place

open Outcome.Syntax

(* List.map2 for a function that returns its failure. It stops at the first
   failure. Lists of different lengths end in Invalid_argument as they do with
   List.map2, after the pairs they have in common. Two function pointer types
   with different numbers of parameters get here (issue #85). *)
let rec map2_result (f: 'a -> 'b -> ('c, 'e) result) (l1: 'a list) (l2: 'b list): ('c list, 'e) result =
  match (l1, l2) with
  | ([], []) ->
     Ok []
  | (a1 :: l1, a2 :: l2) ->
     let* r = f a1 a2 in
     let* rs = map2_result f l1 l2 in
     Ok (r :: rs)
  | ([], _ :: _) | (_ :: _, []) ->
     invalid_arg "List.map2"

(* A function named try_ returns a failed match as a value, the error that
   reports it. Two callers go on after a failure. The instance lookup tries
   the next instance, and a cast reports an error of its own. Every other
   caller uses the raising functions after this group. *)
let rec try_match_type (ctx: ctx) (a: ty) (b: ty): (type_bindings, error) result =
  match a with
  | Unit ->
     (match b with
      | Unit ->
         Ok empty_bindings
      | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
      | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | Boolean ->
     (match b with
      | Boolean ->
         Ok empty_bindings
      | Unit | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
      | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ ->
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
      | FnPtr _ | MonoTy _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | SingleFloat ->
     (match b with
      | SingleFloat ->
         Ok empty_bindings
      | Unit | Boolean | Integer _ | DoubleFloat | NamedType _ | RegionTy _ | ReadRef _
      | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
      | MonoTy _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | DoubleFloat ->
     (match b with
      | DoubleFloat ->
         Ok empty_bindings
      | Unit | Boolean | Integer _ | SingleFloat | NamedType _ | RegionTy _ | ReadRef _
      | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
      | MonoTy _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | NamedType (n, args, _) ->
     (match b with
      | NamedType (n', args', _) ->
         (* We ignore the universe, since the type system is nominal. *)
         if n = n' then
           try_match_type_list ctx args args'
         else
           Error (Type (TypeMismatch { expected = a; actual = b }))
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _ | ReadRef _
      | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
      | MonoTy _ ->
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
      | FnPtr _ | MonoTy _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | ReadRef (t, r) ->
     (match b with
      | ReadRef (t', r') ->
         let* bindings = try_match_type ctx t t' in
         let* bindings' = try_match_type ctx (ty_of_ref_region r) (ty_of_ref_region r') in
         try_merge_bindings bindings bindings'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | WriteRef (t, r) ->
     (match b with
      | WriteRef (t', r') ->
         let* bindings = try_match_type ctx t t' in
         let* bindings' = try_match_type ctx (ty_of_ref_region r) (ty_of_ref_region r') in
         try_merge_bindings bindings bindings'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | Span (t, r) ->
     (match b with
      | Span (t', r') ->
         let* bindings = try_match_type ctx t t' in
         let* bindings' = try_match_type ctx (ty_of_ref_region r) (ty_of_ref_region r') in
         try_merge_bindings bindings bindings'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | SpanMut _ | TyVar _ | Address _
      | Pointer _ | FnPtr _ | MonoTy _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | SpanMut (t, r) ->
     (match b with
      | SpanMut (t', r') ->
         let* bindings = try_match_type ctx t t' in
         let* bindings' = try_match_type ctx (ty_of_ref_region r) (ty_of_ref_region r') in
         try_merge_bindings bindings bindings'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | TyVar tyvar ->
     try_match_type_var ctx tyvar b
  | Address t ->
     (match b with
      | Address t' ->
         try_match_type ctx t t'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Pointer _
      | FnPtr _ | MonoTy _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | Pointer t ->
     (match b with
      | Pointer t' ->
         try_match_type ctx t t'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _
      | FnPtr _ | MonoTy _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | FnPtr (args, rt) ->
     (match b with
      | FnPtr (args', rt') ->
         try_match_type_list ctx (rt :: args) (rt' :: args')
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _
      | Pointer _ | MonoTy _ ->
         Error (Type (TypeMismatch { expected = a; actual = b })))
  | MonoTy _ ->
     Error (Generic MatchTypeOnMonoTy)

and try_match_type_var (ctx: ctx) (tv: type_var) (ty: ty): (type_bindings, error) result =
  let (TypeVariable (name, universe, from, constraints)) = tv in
  (* Check if the argument type is a variable. *)
  match ty with
  | (TyVar (TypeVariable (i', u', from', constraints'))) ->
     (* When the argument type is a type variable, check if the variables have
        the same name and provenance. *)
     if (equal_identifier name i') && (universe = u') && (equal_qident from from') then
       (* If so, do nothing: we don't want circular bindings *)
       Ok empty_bindings
     else
       (* Check that the tyvar implements the type variable's constraints, if any. *)
       let _ = check_tyvar_implements_constraints constraints constraints' in
       (* Otherwise, add a new binding.

          The idea here is: suppose we have a function `f` that accepts an
          argument of a generic type `T`. Now we're type-checking a function `g`
          that accepts an argument `x` of generic type `U`. If we encounter a
          function call `f(x)`, then the parameter type is `T` and the argument
          type is `U`, so we create a binding `T -> U`.  *)
       try_add_binding empty_bindings (tyvar_to_typaram tv) ty
  | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
  | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | Address _ | Pointer _ | FnPtr _
  | MonoTy _ ->
     (* Check that the type implements the type variable's constraints, if any. *)
     let* () = check_type_implements_constraints ctx tv ty constraints in
     (* If the constraints are satisfied, add a straightforward binding. *)
     try_add_binding empty_bindings (tyvar_to_typaram tv) ty

and check_tyvar_implements_constraints (param: sident list) (arg: sident list): bool =
  (* Check that the param is a subset of the param. *)
  let param: SIdentSet.t = SIdentSet.of_list param
  and arg: SIdentSet.t = SIdentSet.of_list arg
  in
  SIdentSet.subset param arg

and check_type_implements_constraints (ctx: ctx) (tv: type_var) (ty: ty) (constraints: sident list): (unit, error) result =
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

and try_constraint (ctx: ctx) (tv: type_var) (ty: ty) (typeclass_name: sident): (unit, error) result =
  (* get_decl_by_name raises when the module of the name is not in the
     environment. The miss is returned here, before it can. *)
  match get_module_by_name (ctx_env ctx) (sident_module_name typeclass_name) with
  | None ->
     Error (Declaration (DeclarationErrors.UnknownModule (sident_module_name typeclass_name)))
  | Some _ ->
  (* Find the typeclass. *)
  match get_decl_by_name (ctx_place ctx) (ctx_env ctx) typeclass_name with
  | Some (TypeClass { id; _ }) ->
     (* Try to find an instance for the given typeclass name that implements this type. *)
     let* instance = try_get_instance (ctx_place ctx) (ctx_env ctx) (ctx_mn ctx) ty id in
     (match instance with
      | Some _ ->
         Ok ()
      | None ->
         Error (Type (UnsatisfiedConstraint { tyvar = tv; ty; typeclass = typeclass_name })))
  (* The next two arms report the constraint where the function is called: the
     name in a type parameter constraint is not looked up where the constraint
     is declared (#128). *)
  | Some (Constant _ | Record _ | Union _ | UnionCase _ | Function _
      | TypeClassMethod _ | Instance _) ->
     Error (Type (ConstraintNotTypeclass { tyvar = tv; typeclass = typeclass_name }))
  | None ->
     Error (Type (ConstraintUnknownTypeclass { tyvar = tv; typeclass = typeclass_name }))

and try_get_instance (place: place) (env: env) (source_module_name: module_name) (dispatch_ty: ty) (typeclass: typeclass_id): ((instance_decl * type_bindings) option, error) result =
  match get_module_by_name env source_module_name with
  | None ->
     Error (Declaration (DeclarationErrors.UnknownModule source_module_name))
  (* visible_instances looks the module up again, by this ID. It was found
     here, so that lookup does not miss. *)
  | Some (ModRec { id = mod_id; _ }) ->
  let pred (decl: instance_decl): (instance_decl * type_bindings) option =
    match decl with
    | { typeclass_id; argument; _ } ->
       if equal_typeclass_id typeclass_id typeclass then
         (match try_match_type (env, source_module_name, place) argument dispatch_ty with
          | Ok bindings ->
             Some (decl, bindings)
          | Error _ ->
             (* Does not match, just skip to the next instance. Every failure
                skips it, also an internal error from checking a constraint of
                the instance's type parameters. *)
             None)
       else
         None
  in
  let filtered: (instance_decl * type_bindings) list =
    (List.filter_map pred (visible_instances place env mod_id))
  in
  match filtered with
  | [a] ->
     Ok (Some a)
  | _::_ ->
     (* The declarations of overlapping instances are not all rejected where
        they are declared (#72, #122), so the call reports them. *)
     let { name; _ }: typeclass_decl = get_typeclass place env typeclass in
     let modules: module_name list =
       List.fold_left
         (fun (names: module_name list) (({ mod_id; _ }: instance_decl), _) ->
           let mn: module_name = module_name_from_id place env mod_id in
           if List.mem mn names then names else names @ [mn])
         []
         filtered
     in
     Error (Type (OverlappingInstancesMatch { typeclass = name; ty = dispatch_ty; modules }))
  | [] ->
     Ok None

and try_match_type_list (ctx: ctx) (tys: ty list) (tys': ty list): (type_bindings, error) result =
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

let match_type (ctx: ctx) (a: ty) (b: ty): type_bindings =
  ok_or_raise (ctx_place ctx) (try_match_type ctx a b)

let get_instance (place: place) (env: env) (source_module_name: module_name) (dispatch_ty: ty) (typeclass: typeclass_id): (instance_decl * type_bindings) option =
  ok_or_raise place (try_get_instance place env source_module_name dispatch_ty typeclass)

let match_typarams (ctx: ctx) (typarams: typarams) (args: ty list): type_bindings =
  let typarams' = List.map (fun tp -> TyVar (typaram_to_tyvar tp)) (typarams_as_list typarams) in
  ok_or_raise (ctx_place ctx) (try_match_type_list ctx typarams' args)

let match_type_with_value (ctx: ctx) (ty: ty) (expr: texpr): type_bindings =
  (* Like match_type, but a literal that the rules for literals accept does not
     have to have the type. *)
  if LiteralTyping.literal_matches (ctx_place ctx) ty expr then
    empty_bindings
  else
    match_type ctx ty (get_type (ctx_place ctx) expr)
