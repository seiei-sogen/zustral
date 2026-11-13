(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)
open Identifier
open Common
open BuiltIn
open Id
open Env
open EnvTypes
open EnvExtras
open Region
open RegionMap
open Type
open TypeSystem
open TypeParser
open Stages.AstDB
open Stages.Tast
open TastUtil
open TypeParameter
open TypeParameters
open TypeVarSet
open LexEnv
open TypeBindings
open TypeMatch
open Error
open Util

(* Expression Context *)

type expr_ctx =
  ExpressionContext of {
      module_name: module_name;
      (** The name of the module being type-checked. *)
      env: env;
      (** The environment. *)
      rm: region_map;
      (** The region map, mapping the names of regions defined in
          borrow statements to region objects. *)
      typarams: typarams;
      (** Type parameters of the function or method containing this expression. *)
      lexenv: lexenv;
      (** The lexical environment. *)
      place: place;
      (** Where an error in the expression is reported. *)
    }

let make_ctx (place: place) (module_name: module_name) (env: env) (rm: region_map) (typarams: typarams) (lexenv: lexenv): expr_ctx =
  ExpressionContext { module_name; env; rm; typarams; lexenv; place }

let ctx_module_name (ctx: expr_ctx): module_name =
  let (ExpressionContext { module_name; _ }) = ctx in
  module_name

let ctx_env (ctx: expr_ctx): env =
  let (ExpressionContext { env; _ }) = ctx in
  env

let ctx_region_map (ctx: expr_ctx): region_map =
  let (ExpressionContext { rm; _ }) = ctx in
  rm

let ctx_typarams (ctx: expr_ctx): typarams =
  let (ExpressionContext { typarams; _ }) = ctx in
  typarams

let ctx_lexenv (ctx: expr_ctx): lexenv =
  let (ExpressionContext { lexenv; _ }) = ctx in
  lexenv

let ctx_place (ctx: expr_ctx): place =
  let (ExpressionContext { place; _ }) = ctx in
  place

(* Utilities *)

(** Wrapper around match_type. *)
let match_type_ctx (ctx: expr_ctx) (expected: ty) (actual: ty): type_bindings =
  let env: env = ctx_env ctx
  and module_name: module_name = ctx_module_name ctx in
  match_type (env, module_name, ctx_place ctx) expected actual

(** Wrapper around match_type_with_value. *)
let match_type_with_value_ctx (ctx: expr_ctx) (expected: ty) (value: texpr): type_bindings =
  let env: env = ctx_env ctx
  and module_name: module_name = ctx_module_name ctx in
  match_type_with_value (env, module_name, ctx_place ctx) expected value

(** Wrapper around match_typarams_ctx. *)
let match_typarams_ctx (ctx: expr_ctx) (typarams: typarams) (type_args: ty list): type_bindings =
  match_typarams (ctx_env ctx, ctx_module_name ctx, ctx_place ctx) typarams type_args

(** Is this expression of boolean type? *)
let is_bool (place: place) (e: texpr): bool =
  match get_type place e with
  | Boolean -> true
  | Unit | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _ | ReadRef _
  | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
  | MonoTy _ -> false

(* Since the extraction pass has already happened, we can simplify the call to
   `parse_type` by passing an empty list of local type signatures. *)
let parse_typespec (ctx: expr_ctx) (ty: qtypespec): ty =
  parse_type (ctx_place ctx) (ctx_env ctx) [] (ctx_region_map ctx) (ctx_typarams ctx) ty

let get_record_definition (place: place) (env: env) (name: qident) (ty: ty): (module_name * type_vis * typarams * typed_slot list) =
  match get_decl_by_name place env (qident_to_sident name) with
  | (Some (Record { mod_id; vis; typarams; slots; _ })) ->
     let mod_name: module_name = module_name_from_id place env mod_id in
     (mod_name, vis, typarams, slots)
  | Some (Constant _ | Union _ | UnionCase _ | Function _ | TypeClass _
      | TypeClassMethod _ | Instance _) ->
     type_error place (PathNotRecord ty)
  | None ->
     generic_error place (NoRecordWithName (original_name name))

let get_slot_with_name (place: place) type_name slots slot_name =
  match List.find_opt (fun (TypedSlot (n, _)) -> n = slot_name) slots with
  | Some s -> s
  | None ->
     type_error place (NoSuchSlot { type_name = original_name type_name; slot_name })

(* Check that there are as many bindings as there are type parameters, and that
   every type parameter is satisfied. *)
and check_bindings (place: place) (typarams: typarams) (bindings: type_bindings): unit =
  let check (tp: type_parameter): unit =
    let n = typaram_name tp
    and u = typaram_universe tp
    in
    (match get_binding bindings tp with
     | Some ty ->
        if universe_compatible u (type_universe place ty) then
          ()
        else
          generic_error place (BindingUniverseMismatch { expected = u; actual = type_universe place ty })
     | None ->
        generic_error place (NoBindingForParameter n))
  in
  let _ = List.map check (typarams_as_list typarams) in
  ()


(* Argument List Checking *)

(* Each parameter with its argument, or None when there are not as many
   arguments as parameters. *)
let rec pair_arguments (params: value_parameter list) (args: texpr list): (value_parameter * texpr) list option =
  match (params, args) with
  | ((first_param::rest_params), (first_arg::rest_args)) ->
     Option.map (fun rest -> (first_param, first_arg) :: rest) (pair_arguments rest_params rest_args)
  | ([], []) ->
     Some []
  | ((_::_), []) | ([], (_::_)) ->
     None

(* Given a list of type parameters, and a list of arguments, check that the
   lists have the same length and each argument satisfies each corresponding
   parameter. Return the resulting set of type bindings. *)
let rec check_argument_list (ctx: expr_ctx) (name: identifier) (params: value_parameter list) (args: texpr list): type_bindings =
  match pair_arguments params args with
  | Some pairs ->
     List.fold_left
       (fun bindings (param, arg) -> merge_bindings (ctx_place ctx) bindings (match_parameter ctx param arg))
       empty_bindings
       pairs
  | None ->
     type_error (ctx_place ctx) (CallWrongArity { name; expected = List.length params; actual = List.length args })

and match_parameter (ctx: expr_ctx) (param: value_parameter) (arg: texpr): type_bindings =
  let (ValueParameter (_, ty)) = param in
  match_type_with_value_ctx ctx ty arg

let make_substs (place: place) (bindings: type_bindings) (typarams: typarams): type_bindings =
  let f (tp: type_parameter): (type_parameter * ty) option =
    match typaram_universe tp with
    | RegionUniverse ->
       Some (tp, RegionTy static_region)
    | FreeUniverse | LinearUniverse | TypeUniverse ->
       (match get_binding bindings tp with
        | Some ty ->
           Some (tp, ty)
        | None ->
           None)
  in
  bindings_from_list place (List.filter_map f (typarams_as_list typarams))

(* Types From the Place of an Expression *)

(* A generic function used as a value, and a call whose return type has a type
   variable that none of its parameters mentions, have type variables that the
   expression does not fix. Only the place the expression is in can fix them. A
   function of this type gives their bindings. Its arguments are the name to
   report and the type that has the variables. `infer` passes
   `no_expected_type` and `check` passes `expected_type`. *)
type context_bindings = identifier -> ty -> type_bindings

(* The place says nothing about the type. *)
let no_expected_type (place: place) (name: identifier) (_: ty): type_bindings =
  type_error place (UnconstrainedGenericFunction name)

(* The place expects the type `expected`: the bindings are the ones that make
   the type of the expression that type. *)
let expected_type (ctx: expr_ctx) (expected: ty) (_: identifier) (generic_ty: ty): type_bindings =
  match_type_ctx ctx generic_ty expected

(* Type Checking: Function Calls *)

let rec augment_call (ctx: expr_ctx) (name: qident) (args: typed_arglist) (from_context: context_bindings): texpr =
  (* First, check if the callable name is a variable. *)
  match get_var (ctx_lexenv ctx) (local_name name) with
  | Some (ty, _) ->
     augment_fptr_call ctx (local_name name) ty from_context args
  | None ->
     (* Otherwise, find a callable from the environment. *)
     (match get_callable (ctx_place ctx) (ctx_env ctx) (ctx_module_name ctx) (qident_to_sident name) with
      | Some callable ->
         augment_callable ctx name callable from_context args
      | None ->
         generic_error (ctx_place ctx) (UnknownName { kind = CallableName; name = original_name name }))

and augment_fptr_call (ctx: expr_ctx) (name: identifier) (fn_ptr_ty: ty) (from_context: context_bindings) (args: typed_arglist): texpr =
  (* Because function pointers don't preserve value parameter names, we can't
     accept named argument lists. *)
  let args: texpr list =
    match args with
    | TPositionalArglist args -> args
    | TNamedArglist _ ->
       type_error (ctx_place ctx) FunPointerNamedArgs
  in
  (* We extract the parameter types and return type from the function pointer
     type. *)
  let (paramtys, rt): (ty list * ty) =
    match fn_ptr_ty with
    | FnPtr (paramtys, rt) -> (paramtys, rt)
    | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
    | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
    | MonoTy _ -> type_error (ctx_place ctx) (CallNonCallable fn_ptr_ty)
  in
  (* Then, we synthesize a parameter list from the function pointer type. The
     parameter names are fake, but this is so we can reuse the arity-checking,
     value parameter list-checking infrastructure we use elsewhere. *)
  let params: value_parameter list = List.map (fun paramty -> ValueParameter (make_ident BuiltInNames.fake_parameter_name, paramty)) paramtys in
  (* Check synthetic parameter list against argument list. *)
  let bindings = check_argument_list ctx name params args in
  (* Use the bindings to get the effective return type *)
  let rt' = replace_variables bindings rt in
  let (bindings', rt'') = handle_return_type_polymorphism (ctx_place ctx) name params rt' from_context bindings in
  let arguments = cast_arguments bindings' params args in
  TFptrCall (name, arguments, rt'')

and augment_callable (ctx: expr_ctx) (name: qident) (callable: callable) (from_context: context_bindings) (args: typed_arglist) =
  match callable with
  | FunctionCallable (id, typarams, params, rt) ->
     augment_function_call ctx id name typarams params rt from_context args
  | RecordConstructor (_, typarams, universe, slots) ->
     augment_record_constructor ctx name typarams universe slots from_context args
  | UnionConstructor { union_id; type_params; universe; case } ->
     let type_name: qident = (let { name; mod_id; _ }: union_decl = get_union (ctx_place ctx) (ctx_env ctx) union_id in
                                 (* TODO: constructing a fake qident *)
                                 make_qident (module_name_from_id (ctx_place ctx) (ctx_env ctx) mod_id, name, name))
     in
     augment_union_constructor ctx union_id type_name type_params universe case from_context args
  | MethodCallable { typeclass_id; value_parameters; return_type; _ } ->
     let { param; _ }: typeclass_decl = get_typeclass (ctx_place ctx) (ctx_env ctx) typeclass_id in
     augment_method_call ctx typeclass_id param name value_parameters return_type from_context args

and augment_function_call (ctx: expr_ctx) (id: function_id) name typarams params rt (from_context: context_bindings) args =
  (* For simplicity, and to reduce duplication of code, we convert the argument
     list to a positional list. *)
  let param_names = List.map (fun (ValueParameter (n, _)) -> n) params in
  let arguments: texpr list = arglist_to_positional (ctx_place ctx) (args, param_names) in
  (* Check the list of params against the list of arguments *)
  let bindings = check_argument_list ctx (original_name name) params arguments in
  (* Use the bindings to get the effective return type *)
  let rt' = replace_variables bindings rt in
  let (bindings', rt'') = handle_return_type_polymorphism (ctx_place ctx) (local_name name) params rt' from_context bindings in
  (* Check: the set of bindings equals the set of type parameters *)
  let bindings'' = merge_bindings (ctx_place ctx) bindings bindings' in
  check_bindings (ctx_place ctx) typarams bindings'';
  let arguments' = cast_arguments bindings'' params arguments in
  let substs = make_substs (ctx_place ctx) bindings'' typarams in
  TFuncall (id, name, arguments', rt'', substs)

and augment_record_constructor (ctx: expr_ctx) (name: qident) (typarams: typarams) (universe: universe) (slots: typed_slot list) (from_context: context_bindings) (args: typed_arglist) =
  (* Check: the argument list must be named *)
  let args' = (match args with
               | TPositionalArglist l ->
                  if l = [] then
                    []
                  else
                    type_error (ctx_place ctx) (ConstructorNotNamed RecordAggregate)
               | TNamedArglist a ->
                  a) in
  (* Check: the set of slots matches the set of param names *)
  let slot_names: identifier list = List.map (fun (TypedSlot (name, _)) -> name) slots in
  let argument_names: identifier list = List.map (fun (n, _) -> n) args' in
  if not (ident_set_eq slot_names argument_names) then
    type_error (ctx_place ctx) (ConstructorWrongArgs RecordAggregate)
  else
    (* Convert args to positional *)
    let arguments = arglist_to_positional (ctx_place ctx) (args, slot_names) in
    (* Check the list of params against the list of arguments *)
    let params = List.map (fun (TypedSlot (n, t)) -> ValueParameter (n, t)) slots in
    let bindings = check_argument_list ctx (original_name name) params arguments in
    (* Use the bindings to get the effective return type *)
    let rt = NamedType (name,
                        List.map (fun tp -> TyVar (typaram_to_tyvar tp)) (typarams_as_list typarams),
                        universe)
    in
    let rt' = replace_variables bindings rt in
    let (bindings', rt'') = handle_return_type_polymorphism (ctx_place ctx) (local_name name) params rt' from_context bindings in
    (* Check: the set of bindings equals the set of type parameters *)
    check_bindings (ctx_place ctx) typarams (merge_bindings (ctx_place ctx) bindings bindings');
    (* Check the resulting type is in the correct universe *)
    if universe_compatible universe (type_universe (ctx_place ctx) rt'') then
      TRecordConstructor (rt'', List.map2 (fun a b -> (a, b)) slot_names arguments)
    else
      generic_error (ctx_place ctx) ConstructorUniverseMismatch

and augment_union_constructor (ctx: expr_ctx) (union_id: union_id) (type_name: qident) (typarams: typarams) (universe: universe) (case: typed_case) (from_context: context_bindings) (args: typed_arglist) =
  let args' = (match args with
               | TPositionalArglist l ->
                  if l = [] then
                    []
                  else
                    type_error (ctx_place ctx) (ConstructorNotNamed UnionAggregate)
               | TNamedArglist a ->
                  a) in
  (* Check: the set of slots matches the set of param names *)
  let (TypedCase (case_name, slots)) = case in
  let slot_names: identifier list = List.map (fun (TypedSlot (name, _)) -> name) slots in
  let argument_names: identifier list = List.map (fun (n, _) -> n) args' in
  if not (ident_set_eq slot_names argument_names) then
    type_error (ctx_place ctx) (ConstructorWrongArgs UnionAggregate)
  else
    (* Convert args to positional *)
    let arguments = arglist_to_positional (ctx_place ctx) (args, slot_names) in
    (* Check the list of params against the list of arguments *)
    let params = List.map (fun (TypedSlot (n, t)) -> ValueParameter (n, t)) slots in
    let bindings = check_argument_list ctx case_name params arguments in
    (* Use the bindings to get the effective return type *)
    let tyargs = List.map (fun tp -> TyVar (typaram_to_tyvar tp)) (typarams_as_list typarams) in
    let rt = NamedType (type_name, tyargs, universe) in
    let rt' = replace_variables bindings rt in
    let (bindings', rt'') = handle_return_type_polymorphism (ctx_place ctx) case_name params rt' from_context bindings in
    (* Check: the set of bindings equals the set of type parameters *)
    let bindings'' = merge_bindings (ctx_place ctx) bindings bindings' in
    check_bindings (ctx_place ctx) typarams bindings'';
    (* The arguments of rt'': rt' replaced the variables of rt with bindings,
       and rt'' those of rt' with bindings that bindings'' holds. The universe
       of rt'' is not the declared one when that is `Type`: each replacement
       computed it again from the arguments. *)
    let tyargs = List.map (fun a -> replace_variables bindings'' (replace_variables bindings a)) tyargs in
    let rt_universe = type_universe (ctx_place ctx) rt'' in
    (* Check the resulting type is in the correct universe *)
    if universe_compatible universe rt_universe then
      TUnionConstructor {
          union_id;
          type_name;
          tyargs;
          universe = rt_universe;
          case_name;
          args = List.map2 (fun a b -> (a, b)) slot_names arguments;
        }
    else
      generic_error (ctx_place ctx) ConstructorUniverseMismatch

and augment_method_call (ctx: expr_ctx) (typeclass_id: typeclass_id) (typaram: type_parameter) (callable_name: qident) (params: value_parameter list) (rt: ty) (from_context: context_bindings) (args: typed_arglist): texpr =
  (* At this point, we know the method's name and the typeclass it belongs
     to. We pull the parameter list from the typeclass, and compare that against
     the argument list. *)
  (* For simplicity, and to reduce duplication of code, we convert the argument
     list to a positional list. *)
  let param_names = List.map (fun (ValueParameter (n, _)) -> n) params in
  let arguments: texpr list = arglist_to_positional (ctx_place ctx) (args, param_names) in
  (* Check the list of params against the list of arguments *)
  let bindings = check_argument_list ctx (original_name callable_name) params arguments in
  (* Use the bindings to get the effective return type *)
  let rt' = replace_variables bindings rt in
  let (bindings', rt'') = handle_return_type_polymorphism (ctx_place ctx) (local_name callable_name) params rt' from_context bindings in
  let bindings'' = merge_bindings (ctx_place ctx) bindings bindings' in
  (* Check: the set of bindings equals the set of type parameters *)
  check_bindings (ctx_place ctx) (typarams_from_list (ctx_place ctx) [typaram]) bindings'';
  match get_binding bindings'' typaram with
  | (Some dispatch_ty) ->
     (* Is the dispatch type a type variable? *)
     (match dispatch_ty with
      | TyVar (TypeVariable (typaram, _, _, constraints)) ->
         (* If so, check if the constraints say that it implements the typeclass. *)
         let tc_name: sident = get_decl_sident_or_die (ctx_place ctx) (ctx_env ctx) typeclass_id in
         if List.exists (fun c -> equal_sident tc_name c) constraints then
           (* If it's a tyvar that implements the typeclass, we have to
              proceed differently. Essentially we can't go on with instance
              resolution, because we don't have a type to resolve an
              instance for. So we have to freeze the process. *)
           TVarMethodCall {
               source_module_name = (ctx_module_name ctx);
               typeclass_id = typeclass_id;
               params = params;
               method_name = callable_name;
               args = arguments;
               dispatch_ty = dispatch_ty;
               rt = rt'';
               bindings = bindings'';
             }
         else
           (* If it doesn't, that's an error *)
           type_error (ctx_place ctx) (UnconstrainedTypeParameter { typeclass = sident_name tc_name; typaram })
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | Address _
      | Pointer _ | FnPtr _ | MonoTy _ ->
         (* If it's not a type variable, continue normal instance resolution. *)
         let (instance, instance_bindings): instance_decl * type_bindings =
           (match get_instance (ctx_place ctx) (ctx_env ctx) (ctx_module_name ctx) dispatch_ty typeclass_id with
            | Some (i, b) -> (i, b)
            | None ->
               let tc_name: sident = get_decl_sident_or_die (ctx_place ctx) (ctx_env ctx) typeclass_id in
               type_error (ctx_place ctx) (NoSuitableInstance { typeclass = sident_name tc_name; ty = dispatch_ty }))
         in
         let params' = List.map (fun (ValueParameter (n, t)) -> ValueParameter (n, replace_variables instance_bindings t)) params in
         let arguments' = cast_arguments instance_bindings params' arguments in
         let { typarams; id = instance_id; argument; _ }: instance_decl = instance in
         let meth_id: ins_meth_id =
           (match get_instance_method_from_instance_id_and_method_name (ctx_env ctx) instance_id (original_name callable_name) with
            | Some (InsMethRec { id; _ }) -> id
            (* An instance that leaves out a method of its typeclass is
               accepted where it is declared (#91), so the call reports it. *)
            | None ->
               let tc_name: sident = get_decl_sident_or_die (ctx_place ctx) (ctx_env ctx) typeclass_id in
               type_error (ctx_place ctx) (InstanceMissingMethod { typeclass = sident_name tc_name; ty = argument; method_name = original_name callable_name }))
         in
         let substs = make_substs (ctx_place ctx) instance_bindings typarams in
         TMethodCall (meth_id, callable_name, typarams, arguments', rt'', substs))
  | None ->
     (* check_bindings above has found that the bindings bind exactly the
        parameter of the typeclass. The lookup still returns an option. *)
     internal_err (ctx_place ctx) "couldn't extract dispatch type."

and cast_arguments (bindings: type_bindings) (params: value_parameter list) (arguments: texpr list): texpr list =
  let f (ValueParameter (_, expected)) value =
    let expected' = replace_variables bindings expected in
    match expected' with
    | Integer _ ->
       (* Cast integer types *)
       TImplicitCast (value, expected')
    | Unit | Boolean | SingleFloat | DoubleFloat | NamedType _ | RegionTy _ | ReadRef _
    | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
    | MonoTy _ ->
       value
  in
  List.map2 f params arguments

and handle_return_type_polymorphism (place: place) (name: identifier) (params: value_parameter list) (rt: ty) (from_context: context_bindings) (bindings: type_bindings): (type_bindings * ty) =
  if is_return_type_polymorphic params rt then
    let bindings' = from_context name rt in
    let bindings'' = merge_bindings place bindings bindings' in
    (bindings'', replace_variables bindings'' rt)
  else
    (empty_bindings, replace_variables bindings rt)

(* Given a function's parameter list and return type, check if the return type
   of a function is polymorphic. *)
and is_return_type_polymorphic (params: value_parameter list) (rt: ty): bool =
  (* Find the set of type variables in the return type. *)
  let rt_type_vars: TypeVarSet.t = type_variables rt in
  (* Find the set of type variables in each of the parameters. *)
  let param_type_vars: TypeVarSet.t list = List.map (fun (ValueParameter (_, ty)) -> type_variables ty) params in
  (* Union the type variables of the parameters into one set of all type
     variables in the parameter list. *)
  let param_type_vars: TypeVarSet.t = List.fold_left TypeVarSet.union TypeVarSet.empty param_type_vars in
  (* Find the set of type variables that are in the return type set but not in
     the parameter set. *)
  let vars_not_params: TypeVarSet.t =
    let param_ty_vars: type_var list = TypeVarSet.elements param_type_vars
    in
    let var_name_in_typarams (n: identifier): bool =
      List.exists (fun (TypeVariable (n', _, _, _)) -> equal_identifier n n') param_ty_vars
    in
    TypeVarSet.of_list (List.filter (fun (TypeVariable (n, _, _, _)) -> not (var_name_in_typarams n)) (TypeVarSet.elements rt_type_vars))
  in
  (TypeVarSet.cardinal vars_not_params) > 0

(* Type Checking: Interface *)

let rec infer (ctx: expr_ctx) (expr: aexpr): texpr =
  (* Dispatch on the AST *)
  match expr with
  | NilConstant ->
     TNilConstant
  | BoolConstant b ->
     TBoolConstant b
  | IntConstant i ->
     TIntConstant i
  | FloatConstant f ->
     TFloatConstant f
  | StringConstant s ->
     TStringConstant s
  | Variable name ->
     augment_variable ctx name (no_expected_type (ctx_place ctx))
  | Temporary name ->
     augment_temporary ctx name
  | FunctionCall (name, args) ->
     let arglist: typed_arglist = augment_arglist ctx args in
     augment_call ctx name arglist (no_expected_type (ctx_place ctx))
  | ArithmeticExpression (op, lhs, rhs) ->
     augment_arithmetic ctx op lhs rhs (no_expected_type (ctx_place ctx))
  | Comparison (op, lhs, rhs) ->
     augment_comparison ctx op lhs rhs
  | Conjunction (lhs, rhs) ->
     augment_conjunction ctx lhs rhs
  | Disjunction (lhs, rhs) ->
     augment_disjunction ctx lhs rhs
  | Negation e ->
     augment_negation ctx e
  | IfExpression (c, t, f) ->
     augment_if_expr ctx c t f
  | Embed (ty, expr, args) ->
     TEmbed (
         parse_typespec ctx ty,
         expr,
         List.map (fun e -> infer ctx e) args
       )
  | Deref expr ->
     augment_deref ctx expr
  | Typecast (expr, ty) ->
     check_annotated ctx expr (parse_typespec ctx ty)
  | SizeOf ty ->
     TSizeOf (parse_typespec ctx ty)
  | SlotAccessor (expr, slot_name) ->
     augment_slot_access ctx expr slot_name
  | PointerSlotAccessor (expr, slot_name) ->
     augment_pointer_slot_access ctx expr slot_name
  | ArrayIndex (expr, index) ->
     augment_array_index ctx expr index

and check (ctx: expr_ctx) (expr: aexpr) (expected: ty): texpr =
  let value: texpr = infer_with_expected ctx expr expected in
  let _ = match_type_with_value_ctx ctx expected value in
  value

(* `expr` in a place that expects the type `expected`, before the two types are
   compared. The expected type reaches a variable, a call and an arithmetic
   expression, where it fixes the type variables that the expression does not.
   Every other expression is inferred as if nothing were expected, and so are
   the arguments and the operands of those three. *)
and infer_with_expected (ctx: expr_ctx) (expr: aexpr) (expected: ty): texpr =
  match expr with
  | Variable name ->
     augment_variable ctx name (expected_type ctx expected)
  | FunctionCall (name, args) ->
     let arglist: typed_arglist = augment_arglist ctx args in
     augment_call ctx name arglist (expected_type ctx expected)
  | ArithmeticExpression (op, lhs, rhs) ->
     augment_arithmetic ctx op lhs rhs (expected_type ctx expected)
  | NilConstant | BoolConstant _ | IntConstant _ | FloatConstant _ | StringConstant _
  | Temporary _ | Comparison _ | Conjunction _ | Disjunction _ | Negation _
  | IfExpression _ | Embed _ | Deref _ | Typecast _ | SizeOf _ | SlotAccessor _
  | PointerSlotAccessor _ | ArrayIndex _ ->
     infer ctx expr

(* Type Checking: Internals *)

and augment_variable (ctx: expr_ctx) (name: qident) (from_context: context_bindings): texpr =
  let env = ctx_env ctx in
  (* Does this name a declaration? *)
  match get_decl_by_name (ctx_place ctx) env (qident_to_sident name) with
  | Some (Function { id; typarams; value_params; rt; _ }) ->
     let arg_tys: ty list = List.map (fun (ValueParameter (_, ty)) -> ty) value_params in
     let fn_ty: ty = FnPtr (arg_tys, rt) in
     if (typarams_size typarams > 0) then
       (* Function is generic, the place it is used in fixes its type
          parameters. *)
       let bindings: type_bindings = from_context (original_name name) fn_ty in
       let rt: ty = replace_variables bindings rt
       and arg_tys: ty list = List.map (replace_variables bindings) arg_tys in
       let effective_fn_ty: ty = FnPtr (arg_tys, rt) in
       TFunVar (id, effective_fn_ty, bindings)
     else
       (* Function is concrete. *)
       TFunVar (id, fn_ty, empty_bindings)
  | None | Some (Constant _ | Record _ | Union _ | UnionCase _ | TypeClass _
      | TypeClassMethod _ | Instance _) ->
     (match get_variable (ctx_place ctx) (ctx_env ctx) (ctx_lexenv ctx) name with
      | Some (ty, src) ->
         (match src with
          | VarConstant ->
             TConstVar (name, ty)
          | VarParam ->
             TParamVar ((original_name name), ty)
          | VarLocal _ ->
             TLocalVar ((original_name name), ty))
      | None ->
         generic_error (ctx_place ctx) (UnknownName { kind = VariableName; name = original_name name }))

and augment_temporary (ctx: expr_ctx) (name: identifier): texpr =
  TTemporary (name, get_temporary_type (ctx_place ctx) (ctx_lexenv ctx) name)

and augment_comparison ctx op lhs rhs =
  let lhs' = infer ctx lhs
  and rhs' = infer ctx rhs in
  let _ =
    (* A lone literal on the left takes the type of the right operand. *)
    if (LiteralTyping.is_numeric_literal lhs) && not (LiteralTyping.is_numeric_literal rhs) then
      match_type_with_value_ctx ctx (get_type (ctx_place ctx) rhs') lhs'
    else
      match_type_with_value_ctx ctx (get_type (ctx_place ctx) lhs') rhs'
  in
  TComparison (op, lhs', rhs')

and augment_conjunction ctx lhs rhs =
  let lhs': texpr = infer ctx lhs
  and rhs': texpr = infer ctx rhs in
  if ((is_bool (ctx_place ctx) lhs') && (is_bool (ctx_place ctx) rhs')) then
    TConjunction (lhs', rhs')
  else
    type_error (ctx_place ctx) (LogicalOperandsNotBoolean (ConjunctionOperands (get_type (ctx_place ctx) lhs', get_type (ctx_place ctx) rhs')))

and augment_disjunction ctx lhs rhs =
  let lhs': texpr = infer ctx lhs
  and rhs': texpr = infer ctx rhs in
  if ((is_bool (ctx_place ctx) lhs') && (is_bool (ctx_place ctx) rhs')) then
    TDisjunction (lhs', rhs')
  else
    type_error (ctx_place ctx) (LogicalOperandsNotBoolean (DisjunctionOperands (get_type (ctx_place ctx) lhs', get_type (ctx_place ctx) rhs')))

and augment_negation ctx e =
  let e' = infer ctx e in
  if is_bool (ctx_place ctx) e' then
    TNegation e'
  else
    type_error (ctx_place ctx) (LogicalOperandsNotBoolean (NegationOperand (get_type (ctx_place ctx) e')))

and augment_if_expr ctx c t f =
  let c' = infer ctx c
  and t' = infer ctx t
  and f' = infer ctx f in
  if is_bool (ctx_place ctx) c' then
    if (get_type (ctx_place ctx) t') = (get_type (ctx_place ctx) f') then
      TIfExpression (c', t', f')
    else
      type_error (ctx_place ctx) (IfInequal { lhs = get_type (ctx_place ctx) t'; rhs = get_type (ctx_place ctx) f' })
  else
    type_error (ctx_place ctx) (ConditionNotBoolean { condition = IfExpressionCondition; ty = get_type (ctx_place ctx) c' })

and augment_deref (ctx: expr_ctx) (expr: aexpr): texpr =
  (* The type of the expression being dereferenced must be either a read-only
     reference or a write reference. *)
  let expr' = infer ctx expr in
  let ty = get_type (ctx_place ctx) expr' in
  (match ty with
   | ReadRef (ty', _) ->
      (match type_universe (ctx_place ctx) ty' with
       | FreeUniverse ->
          TDeref (expr', ty')
       | LinearUniverse | TypeUniverse | RegionUniverse ->
          linearity_error (ctx_place ctx) (DereferenceNonFree ty'))
   | WriteRef (ty', _) ->
      (match type_universe (ctx_place ctx) ty' with
       | FreeUniverse ->
          TDeref (expr', ty')
       | LinearUniverse | TypeUniverse | RegionUniverse ->
          linearity_error (ctx_place ctx) (DereferenceNonFree ty'))
   | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
   | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _ | MonoTy _ ->
      type_error (ctx_place ctx) (DereferenceNonReference ty))

and augment_slot_access (ctx: expr_ctx) (expr: aexpr) (slot_name: identifier): texpr =
  (* The type of the expression is a reference. *)
  let expr: texpr = infer ctx expr in
  let ty, region, mode =
    match (get_type (ctx_place ctx) expr) with
    | ReadRef (ty, reg) ->
       (ty, reg, ReadBorrow)
    | WriteRef (ty, reg) ->
       (ty, reg, WriteBorrow)
    | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
    | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _ | MonoTy _ ->
       generic_error (ctx_place ctx) (RefTransformNeedsRef (get_type (ctx_place ctx) expr))
  in
  (* The pointed-to type is a record. *)
  let (type_name, type_args): qident * ty list = begin
      match ty with
      | NamedType (name, args, _) ->
         (name, args)
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _ | ReadRef _
      | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
      | MonoTy _ ->
         type_error (ctx_place ctx) (CantAccessSlotNotRecord { slot_name; ty })
    end
  in
  let (source_module, vis, typarams, slots) = get_record_definition (ctx_place ctx) (ctx_env ctx) type_name ty in
  (* We can access that record. *)
  let _ =
    let public: bool =
      match vis with
      | TypeVisPublic -> true
      | TypeVisOpaque | TypeVisPrivate -> false
    in
    if public || ((ctx_module_name ctx) = source_module) then
      (* The record is public, or it is defined in the same module as we are. *)
      ()
    else
      type_error (ctx_place ctx) (PathNotPublic { type_name = original_name type_name; slot_name })
  in
  (* Get the slot with the given name. *)
  let (TypedSlot (_, slot_ty)) = get_slot_with_name (ctx_place ctx) type_name slots slot_name in
  (* Make the slot type. *)
  let bindings: type_bindings = match_typarams_ctx ctx typarams type_args in
  let slot_ty: ty = replace_variables bindings slot_ty in
  (* The result ty is a reference to the type of the slot. *)
  let ty: ty =
    match mode with
    | ReadBorrow -> ReadRef (slot_ty, region)
    | WriteBorrow -> WriteRef (slot_ty, region)
  in
  (* Return *)
  TSlotAccessor (expr, slot_name, ty)

and augment_pointer_slot_access (ctx: expr_ctx) (expr: aexpr) (slot_name: identifier): texpr =
  (* The type of the expression is a reference... *)
  let expr: texpr = infer ctx expr in
  let ty1: ty = get_type (ctx_place ctx) expr in
  let ty2, region =
    match ty1 with
    | ReadRef (ty2, reg) ->
       (ty2, reg)
    | WriteRef (ty2, reg) ->
       (ty2, reg)
    | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
    | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _ | MonoTy _ ->
       generic_error (ctx_place ctx) (RefTransformNeedsRef (get_type (ctx_place ctx) expr))
  in
  (* ... to a reference... *)
  let (ty3, mode): ty * borrowing_mode =
    match ty2 with
    | ReadRef (ty3, _) ->
       (ty3, ReadBorrow)
    | WriteRef (ty3, _) ->
       (ty3, WriteBorrow)
    | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
    | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _ | MonoTy _ ->
       type_error (ctx_place ctx) (PointerAccessNotReference ty2)
  in
  (* ... to a record. *)
  let (type_name, type_args): qident * ty list = begin
      match ty3 with
      | NamedType (name, args, _) ->
         (name, args)
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _ | ReadRef _
      | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
      | MonoTy _ ->
         type_error (ctx_place ctx) (CantAccessSlotNotRecord { slot_name; ty = ty3 })
    end
  in
  let (source_module, vis, typarams, slots) = get_record_definition (ctx_place ctx) (ctx_env ctx) type_name ty3 in
  (* We can access that record. *)
  let _ =
    let public: bool =
      match vis with
      | TypeVisPublic -> true
      | TypeVisOpaque | TypeVisPrivate -> false
    in
    if public || ((ctx_module_name ctx) = source_module) then
      (* The record is public, or it is defined in the same module as we are. *)
      ()
    else
      type_error (ctx_place ctx) (PathNotPublic { type_name = original_name type_name; slot_name })
  in
  (* Get the slot with the given name. *)
  let (TypedSlot (_, slot_ty)) = get_slot_with_name (ctx_place ctx) type_name slots slot_name in
  (* Make the slot type. *)
  let bindings: type_bindings = match_typarams_ctx ctx typarams type_args in
  let slot_ty: ty = replace_variables bindings slot_ty in
  (* The result ty is a reference to the type of the slot. *)
  let ty: ty =
    match mode with
    | ReadBorrow -> ReadRef (slot_ty, region)
    | WriteBorrow -> WriteRef (slot_ty, region)
  in
  (* Return *)
  TPointerSlotAccessor (expr, slot_name, ty)

and augment_array_index (ctx: expr_ctx) (expr: aexpr) (index_expr: aexpr): texpr =
  (* Augment the expression and the index expression. *)
  let expr: texpr = infer ctx expr
  and index_expr: texpr = infer ctx index_expr in
  (* The type of the expression is a read reference, or a span. *)
  match (get_type (ctx_place ctx) expr) with
  | ReadRef (ty, reg) -> begin
      let elem_ty: ty =
        match ty with
        | Span (ty, _) ->
           ty
        | SpanMut (ty, _) ->
           ty
        | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
        | RegionTy _ | ReadRef _ | WriteRef _ | TyVar _ | Address _ | Pointer _
        | FnPtr _ | MonoTy _ ->
           type_error (ctx_place ctx) (CantIndexNotSpan ty)
      in
      TArrayIndex (expr, index_expr, elem_ty, reg, ReadBorrow)
    end
  | WriteRef (ty, reg) -> begin
      let elem_ty: ty =
        match ty with
        | Span (ty, _) ->
           ty
        | SpanMut (ty, _) ->
           ty
        | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
        | RegionTy _ | ReadRef _ | WriteRef _ | TyVar _ | Address _ | Pointer _
        | FnPtr _ | MonoTy _ ->
           type_error (ctx_place ctx) (CantIndexNotSpan ty)
      in
      TArrayIndex (expr, index_expr, elem_ty, reg, WriteBorrow)
    end
  | Span (ty, reg) ->
     TSpanIndex (expr, index_expr, ty, reg, ReadBorrow)
  | SpanMut (ty, reg) ->
     TSpanIndex (expr, index_expr, ty, reg, WriteBorrow)
  | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
  | TyVar _ | Address _ | Pointer _ | FnPtr _ | MonoTy _ ->
     type_error (ctx_place ctx) (ArrayIndexNotReference (get_type (ctx_place ctx) expr))


and check_annotated (ctx: expr_ctx) (expr: aexpr) (target_type: ty): texpr =
  (* A type annotation has three uses:

     1. Clarifying the type of integer and floating point literals.

     2. Converting write references to read references.

     3. Clarifying the type of return type polymorphic functions.

   *)
  (* By giving target_type as the expected type we're doing point 3. *)
  let expr' = infer_with_expected ctx expr target_type in
  (* For 1: if the expression is an int literal and the target type is an
     int type, do the conversion. Similarly, if the expression is a float
     constant and the target type is a float, do the conversion. *)
  if LiteralTyping.annotation_types_literal (ctx_place ctx) target_type expr' then
    TCast (expr', target_type)
  else
    (* Otherwise, if the expression has a mutable reference type and the
       target type is a read reference, and they point to the same underlying
       type and underlying region, just convert it to a read ref. This does
       point 2. *)
    (match (get_type (ctx_place ctx) expr') with
     | WriteRef (underlying_ty, region) ->
        (match target_type with
         | ReadRef (underlying_ty', region') ->
            (match (equal_ty underlying_ty underlying_ty', equal_ty (ty_of_ref_region region) (ty_of_ref_region region')) with
             | (true, true) ->
                TCast (expr', target_type)
             | (false, true) ->
                type_error (ctx_place ctx) (CastDifferentReferences DifferentUnderlyingType)
             | (true, false) ->
                type_error (ctx_place ctx) (CastDifferentReferences DifferentUnderlyingRegion)
             | (false, false) ->
                type_error (ctx_place ctx) (CastDifferentReferences DifferentTypeAndRegion))
         | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
         | RegionTy _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _
         | Pointer _ | FnPtr _ | MonoTy _ ->
            type_error (ctx_place ctx) CastWriteRefToNonRef)
     | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
     | RegionTy _ | ReadRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
     | FnPtr _ | MonoTy _ ->
        (match try_match_type (ctx_env ctx, ctx_module_name ctx, ctx_place ctx) target_type (get_type (ctx_place ctx) expr') with
         | Ok _ ->
            expr'
         | Error _ ->
            (* Every failure becomes this error, also an internal error from
               checking a constraint of a type variable in the target type. *)
            type_error (ctx_place ctx) (CastInvalid { target = target_type; source = get_type (ctx_place ctx) expr' })))

and augment_arglist (ctx: expr_ctx) (args: abstract_arglist): typed_arglist =
  match args with
  | Positional args' ->
     TPositionalArglist (List.map (fun a -> infer ctx a) args')
  | Named pairs ->
     TNamedArglist (List.map (fun (n, v) -> (n, infer ctx v)) pairs)

and augment_arithmetic (ctx: expr_ctx) (op: arithmetic_operator) (lhs: aexpr) (rhs: aexpr) (from_context: context_bindings): texpr =
  (* Augment operands. *)
  let lhs' = infer ctx lhs
  and rhs' = infer ctx rhs in
  (* Get the types. *)
  let lhs_ty = get_type (ctx_place ctx) lhs'
  and rhs_ty = get_type (ctx_place ctx) rhs'
  in
  (* A lone literal takes the type of the other operand, and has to fit it like
     the initial value of a `let`. Otherwise the left operand decides. *)
  let operand_ty =
    match (LiteralTyping.is_numeric_literal lhs, LiteralTyping.is_numeric_literal rhs) with
    | (true, false) ->
       let _ = match_type_with_value_ctx ctx rhs_ty lhs' in
       rhs_ty
    | (false, true) ->
       let _ = match_type_with_value_ctx ctx lhs_ty rhs' in
       lhs_ty
    | _ ->
       lhs_ty
  in
  let augment_arithmetic_call _ =
    (* Because method calls don't implement the lenient type checking
       rules for arithmetic, we have to cast the arguments to their
       expected types, so that constants are allowed to be used as
       arguments. This means we have to transform a `ty` into a type
       specifier. *)
    let op_name = BuiltInNames.arithmetic_method_name op in
    let op_qname = make_qident (pervasive_module_name, make_ident op_name, make_ident op_name) in
    let args: typed_arglist = TPositionalArglist [
                                  TImplicitCast (lhs', operand_ty);
                                  TImplicitCast (rhs', operand_ty);
                                ]
    in
    augment_call ctx op_qname args from_context
  in
  (* Are the types the same? *)
  if lhs_ty = rhs_ty then
    (* If the types are the same type, check it is a numeric type. *)
    if (is_numeric (ctx_place ctx) lhs_ty) then
      augment_arithmetic_call ()
    else
      type_error (ctx_place ctx) (ArithmeticNotNumeric lhs_ty)
  else
    (* If the types are different, check if at least one operator is a constant.*)
    if (LiteralTyping.is_numeric_literal lhs) <> (LiteralTyping.is_numeric_literal rhs) then
      (* A lone literal was checked when choosing `operand_ty`. Two literals of
         different types are an integer and a float, and neither decides. *)
      augment_arithmetic_call ()
    else
      type_error (ctx_place ctx) (ArithmeticIncompatibleTypes { lhs = lhs_ty; rhs = rhs_ty })
