(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Env
open EnvTypes
open EnvUtils
open Type
open TypeStripping
open MonoType
open MonoTypeBindings
open TypeBindings
open TypeReplace
open TypeParameter
open TypeParameters
open TypeMatch
open ForeignType
open Stages.Tast
open Stages.Mtast
open Stages.Linked
open Id
open Region
open Error

let make_substs2 (bindings: type_bindings) (typarams: typarams): type_bindings =
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
  bindings_from_list NoSpan (List.filter_map f (typarams_as_list typarams))

(* Monomorphize type specifiers *)

let rec monomorphize_ty (env: env) (ty: stripped_ty): (mono_ty * env) =
  match ty with
  | SUnit ->
     (MonoUnit, env)
  | SBoolean ->
     (MonoBoolean, env)
  | SInteger (s, w) ->
     (MonoInteger (s, w), env)
  | SSingleFloat ->
     (MonoSingleFloat, env)
  | SDoubleFloat ->
     (MonoDoubleFloat, env)
  | SRegionTy r ->
     (MonoRegionTy r, env)
  | SReadRef (ty, region) ->
     let (ty, env) = monomorphize_ty env ty in
     let (region, env) = monomorphize_ty env region in
     (MonoReadRef (ty, region), env)
  | SWriteRef (ty, region) ->
     let (ty, env) = monomorphize_ty env ty in
     let (region, env) = monomorphize_ty env region in
     (MonoWriteRef (ty, region), env)
  | SSpan (ty, region) ->
     let (ty, env) = monomorphize_ty env ty in
     let (region, env) = monomorphize_ty env region in
     (MonoSpan (ty, region), env)
  | SSpanMut (ty, region) ->
     let (ty, env) = monomorphize_ty env ty in
     let (region, env) = monomorphize_ty env region in
     (MonoSpanMut (ty, region), env)
  | SAddress ty ->
     let (ty, env) = monomorphize_ty env ty in
     (MonoAddress ty, env)
  | SPointer ty ->
     let (ty, env) = monomorphize_ty env ty in
     (MonoPointer ty, env)
  | SNamedType (name, args) ->
     let (args, env) = monomorphize_ty_list env args in
     (match get_decl_by_name NoSpan env (qident_to_sident name) with
      | Some decl ->
         (match decl with
          | Record { id; typarams; _ } ->
             let typarams: type_parameter list = typarams_as_list typarams in
             let pairs: (type_parameter * mono_ty) list = List.map2 (fun tp mt -> (tp, mt)) typarams args in
             let bindings: mono_type_bindings = mono_bindings_from_list pairs in
             let (env, mono_id) = add_or_get_record_monomorph env id bindings in
             (MonoNamedType (mono_id_of_record mono_id), env)
          | Union { id; typarams; _ } ->
             let typarams: type_parameter list = typarams_as_list typarams in
             let pairs: (type_parameter * mono_ty) list = List.map2 (fun tp mt -> (tp, mt)) typarams args in
             let bindings: mono_type_bindings = mono_bindings_from_list pairs in
             let (env, mono_id) = add_or_get_union_monomorph env id bindings in
             (MonoNamedType (mono_id_of_union mono_id), env)
          | Constant _ | UnionCase _ | Function _ | TypeClass _ | TypeClassMethod _
          | Instance _ ->
             (* A program reaches this: a union case declared after a record or
                union of the same name is the one a lookup by name finds (#86). *)
             internal_err NoSpan ("named type `" ^ (qident_debug_name name) ^ "`points to something that isn't a type"))
      | None ->
         (* No type rules this out. A named type holds the name of its
            declaration, not an ID: the type parser resolves a type of the
            module being extracted from the signatures of that module, before
            the declaration is registered and has an ID. *)
         internal_err NoSpan ("Couldn't get declaration for named type `" ^ (qident_debug_name name) ^ "`."))
  | SFnPtr (args, rt) ->
     let (args, env) = monomorphize_ty_list env args in
     let (rt, env) = monomorphize_ty env rt in
     (MonoFnPtr (args, rt), env)
  | SMonoTy id ->
     (MonoNamedType id, env)
  | SRegionTyVar (name, source) ->
     (MonoRegionTyVar (name, source), env)

and monomorphize_ty_list (env: env) (tys: stripped_ty list): (mono_ty list * env) =
  match tys with
  | first::rest ->
     let (first, env) = monomorphize_ty env first in
     let (rest, env) = monomorphize_ty_list env rest in
     (first :: rest, env)
  | [] ->
     ([], env)

let strip_and_mono (env: env) (ty: ty): (mono_ty * env) =
  let ty = strip_type ty in
  monomorphize_ty env ty

(* The types a foreign signature points to are parts of the parameter and return
   types, which are monomorphized first, so this registers no new monomorph. *)
let monomorphize_foreign_signature (env: env) (signature: ty foreign_signature): (mono_ty foreign_signature * env) =
  map_signature strip_and_mono env signature

let strip_and_mono_list (env: env) (tys: ty list): (mono_ty list * env) =
  let tys = List.map strip_type tys in
  monomorphize_ty_list env tys

(* Monomorphize expressions *)

let rec monomorphize_expr (env: env) (expr: texpr): (mexpr * env) =
  match expr with
  | TNilConstant ->
     (MNilConstant, env)
  | TBoolConstant b ->
     (MBoolConstant b, env)
  | TIntConstant i ->
     (MIntConstant i, env)
  | TFloatConstant f ->
     (MFloatConstant f, env)
  | TStringConstant s ->
     (MStringConstant s, env)
  | TConstVar (name, ty) ->
     let (ty, env) = strip_and_mono env ty in
     (MConstVar (name, ty), env)
  | TParamVar (name, ty) ->
     let (ty, env) = strip_and_mono env ty in
     (MParamVar (name, ty), env)
  | TLocalVar (name, ty) ->
     let (ty, env) = strip_and_mono env ty in
     (MLocalVar (name, ty), env)
  | TTemporary (name, ty) ->
     let (ty, env) = strip_and_mono env ty in
     (MTemporary (name, ty), env)
  | TFunVar (decl_id, ty, bindings) ->
     (* Monomorphize the type. *)
     let (rt, env) = strip_and_mono env ty in
     (* Does the funcall have a substitution list? *)
     if List.length (bindings_list bindings) > 0 then
       (* The function is generic. *)
       (* Monomorphize the tyargs *)
       let typarams = List.map (fun (tp, _) -> tp) (bindings_list bindings) in
       let tyargs = List.map (fun (_, ty) -> strip_type ty) (bindings_list bindings) in
       let (tyargs, env) = monomorphize_ty_list env tyargs in
       let substs: mono_type_bindings = mono_bindings_from_list (List.map2 (fun tp mt -> (tp, mt)) typarams tyargs) in
       let (env, mono_id) = add_or_get_function_monomorph env decl_id substs in
       (MGenericFunVar (mono_id, rt), env)
     else
       (* The function is concrete. *)
       (MConcreteFunVar (decl_id, rt), env)
  | TFuncall (decl_id, name, args, rt, substs) ->
     (* Monomorphize the return type. *)
     let (rt, env) = strip_and_mono env rt in
     (* Monomorphize the arglist *)
     let (args, env) = monomorphize_expr_list env args in
     (* Does the funcall have a substitution list? *)
     if List.length (bindings_list substs) > 0 then
       (* The function is generic. *)
       (* Monomorphize the tyargs *)
       let typarams = List.map (fun (tp, _) -> tp) (bindings_list substs) in
       let tyargs = List.map (fun (_, ty) -> strip_type ty) (bindings_list substs) in
       let (tyargs, env) = monomorphize_ty_list env tyargs in
       let substs: mono_type_bindings = mono_bindings_from_list (List.map2 (fun tp mt -> (tp, mt)) typarams tyargs) in
       let (env, mono_id) = add_or_get_function_monomorph env decl_id substs in
       (MGenericFuncall (mono_id, args, rt), env)
     else
       (* The function is concrete. *)
       (MConcreteFuncall (decl_id, name, args, rt), env)
  | TMethodCall (ins_meth_id, name, typarams, args, rt, substs) ->
     ((* Monomorphize the return type. *)
      let (rt, env) = strip_and_mono env rt in
      (* Monomorphize the arglist *)
      let (args, env) = monomorphize_expr_list env args in
      (* Does the funcall have a list of type params? *)
      if typarams_size typarams > 0 then
        (* The instance is generic. *)
        (* Monomorphize the tyargs *)
        let typarams = List.map (fun (tp, _) -> tp) (bindings_list substs) in
        let tyargs = List.map (fun (_, ty) -> strip_type ty) (bindings_list substs) in
        let (tyargs, env) = monomorphize_ty_list env tyargs in
        let substs: mono_type_bindings = mono_bindings_from_list (List.map2 (fun tp mt -> (tp, mt)) typarams tyargs) in
        let (env, mono_id) = add_or_get_instance_method_monomorph env ins_meth_id substs in
        (MGenericMethodCall (ins_meth_id, mono_id, args, rt), env)
      else
        (* The instance is concrete. *)
        (MConcreteMethodCall (ins_meth_id, name, args, rt), env))
  | TVarMethodCall { dispatch_ty; typeclass_id; source_module_name; params; args; method_name; rt; _ } ->
     (* Continue the instance resolution process we halted in the typing pass. *)
     let (instance, instance_bindings): instance_decl * type_bindings =
       (match get_instance NoSpan env source_module_name dispatch_ty typeclass_id with
        | Some (i, b) -> (i, b)
        | None ->
           generic_error NoSpan (TypeclassResolutionFailed dispatch_ty))
     in
     let params' = List.map (fun (ValueParameter (n, t)) -> ValueParameter (n, replace_variables instance_bindings t)) params in
     let arguments' = TypeCheckExpr.cast_arguments instance_bindings params' args in
     let { typarams; id = instance_id; _ }: instance_decl = instance in
     let meth_id: ins_meth_id =
       (match get_instance_method_from_instance_id_and_method_name env instance_id (original_name method_name) with
        | Some (InsMethRec { id; _ }) -> id
        (* A program reaches this: an instance that leaves out a method of its
           typeclass is accepted where it is declared (#91). *)
        | None -> internal_err NoSpan ("Couldn't find instance method from instance ID `"
                                ^ (show_instance_id instance_id)
                                ^ "` and method name `"
                                ^ (method_name |> original_name |> ident_string)
                                ^ "`."))
     in
     let substs = make_substs2 instance_bindings typarams in
     let mcall = TMethodCall (meth_id, method_name, typarams, arguments', rt, substs) in
     monomorphize_expr env mcall
  | TFptrCall (name, args, ty) ->
     let (args, env) = monomorphize_expr_list env args in
     let (ty, env) = strip_and_mono env ty in
     (MFptrCall (name, args, ty), env)
  | TCast (expr, ty) | TImplicitCast (expr, ty) ->
     let (ty, env) = strip_and_mono env ty in
     let (expr, env) = monomorphize_expr env expr in
     (MCast (expr, ty), env)
  | TComparison (oper, lhs, rhs) ->
     let (lhs, env) = monomorphize_expr env lhs in
     let (rhs, env) = monomorphize_expr env rhs in
     (MComparison (oper, lhs, rhs), env)
  | TConjunction (lhs, rhs) ->
     let (lhs, env) = monomorphize_expr env lhs in
     let (rhs, env) = monomorphize_expr env rhs in
     (MConjunction (lhs, rhs), env)
  | TDisjunction (lhs, rhs) ->
     let (lhs, env) = monomorphize_expr env lhs in
     let (rhs, env) = monomorphize_expr env rhs in
     (MDisjunction (lhs, rhs), env)
  | TNegation expr ->
     let (expr, env) = monomorphize_expr env expr in
     (MNegation expr, env)
  | TIfExpression (c, t, f) ->
     let (c, env) = monomorphize_expr env c in
     let (t, env) = monomorphize_expr env t in
     let (f, env) = monomorphize_expr env f in
     (MIfExpression (c, t, f), env)
  | TRecordConstructor (ty, args) ->
     let (ty, env) = strip_and_mono env ty in
     let (args, env) = monomorphize_named_expr_list env args in
     (MRecordConstructor (ty, args), env)
  | TUnionConstructor (ty, case_name, args) ->
     let (ty, env) = strip_and_mono env ty in
     let (args, env) = monomorphize_named_expr_list env args in
     (MUnionConstructor (ty, case_name, args), env)
  | TEmbed (ty, fmt, args) ->
     let (ty, env) = strip_and_mono env ty in
     let (args, env) = monomorphize_expr_list env args in
     (MEmbed (ty, fmt, args), env)
  | TDeref expr ->
     let (expr, env) = monomorphize_expr env expr in
     (MDeref expr, env)
  | TSizeOf ty ->
     let (ty, env) = strip_and_mono env ty in
     (MSizeOf ty, env)
  | TSlotAccessor (expr, name, ty) ->
     let (expr, env) = monomorphize_expr env expr in
     let (ty, env) = strip_and_mono env ty in
     (MSlotAccessor (expr, name, ty), env)
  | TPointerSlotAccessor (expr, name, ty) ->
     let (expr, env) = monomorphize_expr env expr in
     let (ty, env) = strip_and_mono env ty in
     (MPointerSlotAccessor (expr, name, ty), env)
  | TArrayIndex (expr, idx, ty) ->
     let (expr, env) = monomorphize_expr env expr in
     let (idx, env) = monomorphize_expr env idx in
     let (ty, env) = strip_and_mono env ty in
     (MArrayIndex (expr, idx, ty), env)
   | TSpanIndex (expr, idx, ty) ->
     let (expr, env) = monomorphize_expr env expr in
     let (idx, env) = monomorphize_expr env idx in
     let (ty, env) = strip_and_mono env ty in
     (MSpanIndex (expr, idx, ty), env)

and monomorphize_expr_list (env: env) (exprs: texpr list): (mexpr list * env) =
  match exprs with
  | first::rest ->
     let (first, env) = monomorphize_expr env first in
     let (rest, env) = monomorphize_expr_list env rest in
     (first :: rest, env)
  | [] ->
     ([], env)

and monomorphize_named_expr_list (env: env) (exprs: (identifier * texpr) list): ((identifier * mexpr) list * env) =
  match exprs with
  | (name, first)::rest ->
     let (first, env) = monomorphize_expr env first in
     let (rest, env) = monomorphize_named_expr_list env rest in
     ((name, first) :: rest, env)
  | [] ->
     ([], env)

(* Monomorphize statements *)

let rec monomorphize_stmt (env: env) (stmt: tstmt): (mstmt * env) =
  match stmt with
  | TSkip _ ->
     (MSkip, env)
  | TLet (_, _, name, ty, body) ->
     let (ty, env) = strip_and_mono env ty in
     let (body, env) = monomorphize_stmt env body in
     (MLet (name, ty, body), env)
  | TDestructure (_, _, bindings, value, body) ->
     let (bindings, env) = monomorphize_binding_list env (List.map (fun (TypedBinding { name; ty; rename; }) -> (name, strip_type ty, rename)) bindings) in
     let (value, env) = monomorphize_expr env value in
     let (body, env) = monomorphize_stmt env body in
     (MDestructure (bindings, value, body), env)
  | TAssign (_, lvalue, rvalue) ->
     let (lvalue, env) = monomorphize_expr env lvalue in
     let (rvalue, env) = monomorphize_expr env rvalue in
     (MAssign (lvalue, rvalue), env)
  | TAssignVar (_, name, rvalue) ->
     let (rvalue, env) = monomorphize_expr env rvalue in
     (MAssignVar (name, rvalue), env)
  | TInitialAssign (name, rvalue) ->
     let (rvalue, env) = monomorphize_expr env rvalue in
     (MInitialAssign (name, rvalue), env)
  | TIf (_, c, t, f) ->
     let (c, env) = monomorphize_expr env c in
     let (t, env) = monomorphize_stmt env t in
     let (f, env) = monomorphize_stmt env f in
     (MIf (c, t, f), env)
  | TCase (_, value, whens, case_ref) ->
     let (value, env) = monomorphize_expr env value in
     let (whens, env) = monomorphize_whens env whens in
     (MCase (value, whens, case_ref), env)
  | TWhile (_, value, body) ->
     let (value, env) = monomorphize_expr env value in
     let (body, env) = monomorphize_stmt env body in
     (MWhile (value, body), env)
  | TFor (_, name, start, final, body) ->
     let (start, env) = monomorphize_expr env start in
     let (final, env) = monomorphize_expr env final in
     let (body, env) = monomorphize_stmt env body in
     (MFor (name, start, final, body), env)
  | TBorrow { span; original; rename; region; orig_type; ref_type; body; mode } ->
     let _ = span in
     let (orig_type, env) = strip_and_mono env orig_type in
     let (ref_type, env) = strip_and_mono env ref_type in
     let (body, env) = monomorphize_stmt env body in
     (MBorrow { original = original; rename = rename; region = region; orig_type = orig_type; ref_type = ref_type; body = body; mode = mode }, env)
  | TBlock (_, a, b) ->
     let (a, env) = monomorphize_stmt env a in
     let (b, env) = monomorphize_stmt env b in
     (MBlock (a, b), env)
  | TDiscarding (_, value) ->
     let (value, env) = monomorphize_expr env value in
     (MDiscarding value, env)
  | TReturn (_, value) ->
     let (value, env) = monomorphize_expr env value in
     (MReturn value, env)
  | TLetTmp (name, ty, expr) ->
     let (ty, env) = strip_and_mono env ty in
     let (expr, env) = monomorphize_expr env expr in
     (MLetTmp (name, ty, expr), env)
  | TAssignTmp (name, value) ->
     let (value, env) = monomorphize_expr env value in
     (MAssignTmp (name, value), env)

and monomorphize_whens (env: env) (whens: typed_when list): (mtyped_when list * env) =
  match whens with
  | first::rest ->
     let (first, env) = monomorphize_when env first in
     let (rest, env) = monomorphize_whens env rest in
     (first :: rest, env)
  | [] ->
     ([], env)

and monomorphize_when (env: env) (w: typed_when): (mtyped_when * env) =
  let (TypedWhen (name, params, body)) = w in
  let (bindings, env) = monomorphize_bindings env params in
  let (body, env) = monomorphize_stmt env body in
  (MTypedWhen (name, bindings, body), env)

and monomorphize_bindings (env: env) (bindings: typed_binding list): (mono_binding list * env) =
  match bindings with
  | first::rest ->
     let (TypedBinding { name; ty; rename; }) = first in
     let (ty, env) = monomorphize_ty env (strip_type ty) in
     let (rest, env) = monomorphize_bindings env rest in
     let binding = MonoBinding { name; ty; rename; } in
     (binding :: rest, env)
  | [] ->
     ([], env)

and monomorphize_binding_list (env: env) (bs: (identifier * stripped_ty * identifier) list): (mono_binding list * env) =
  match bs with
  | first::rest ->
     let (name, ty, rename) = first in
     let (ty, env) = monomorphize_ty env ty in
     let (rest, env) = monomorphize_binding_list env rest in
     let binding = MonoBinding { name; ty; rename; } in
     (binding :: rest, env)
  | [] ->
     ([], env)

(* Monomorphize declarations *)

let rec monomorphize_decl (env: env) (decl: typed_decl): (mdecl option * env) =
  match decl with
  | TConstant (id, _, name, ty, value, _) ->
     (* Constant are intrinsically monomorphic, and can be monomorphized
        painlessly. *)
     let (ty, env) = strip_and_mono env ty in
     let (value, env) = monomorphize_expr env value in
     let decl = MConstant (id, name, ty, value) in
     (Some decl, env)
  | TRecord (id, _, name, typarams, _, slots, _) ->
     (* Concrete records are monomorphized immediately. Generic records are
        monomorphized on demand. *)
     if (typarams_size typarams) = 0 then
       let (env, slots) = monomorphize_slots env slots in
       let decl = MRecord (id, name, slots) in
       (Some decl, env)
     else
       (None, env)
  | TUnion (id, _, name, typarams, _, cases, _) ->
     (* Concrete unions are monomorphized immediately. Generic unions are
        monomorphized on demand. *)
     if (typarams_size typarams) = 0 then
       let (env, cases) = Util.map_with_context (fun (e, c) -> monomorphize_case e c) env cases in
       let decl = MUnion (id, name, cases) in
       (Some decl, env)
     else
       (None, env)
  | TFunction (id, _, name, typarams, value_params, rt, body, _) ->
     (* Concrete functions are monomorphized immediately. Generic functions are
        monomorphized on demand. *)
     (if (typarams_size typarams) = 0 then
        let (env, params) = monomorphize_params env value_params in
        let (rt, env) = strip_and_mono env rt in
        let (body, env) = monomorphize_stmt env body in
        let decl = MFunction (id, name, params, rt, body) in
        (Some decl, env)
      else
        (None, env))
  | TForeignFunction (id, _, name, params, rt, underlying, signature, _) ->
     (* Foreign functions are intrinsically monomorphic. *)
     let (env, params) = monomorphize_params env params in
     let (rt, env) = strip_and_mono env rt in
     let (signature, env) = monomorphize_foreign_signature env signature in
     let decl = MForeignFunction (id, name, params, rt, signature, underlying) in
     (Some decl, env)
  | TTypeClass _ ->
     (* Type classes are purely "informative" declarations: they have no physical
        existence in the code. *)
     (None, env)
  | TInstance (decl_id, _, name, typarams, argument, methods, _) ->
     (* Concrete instances can be monomorphized immediately. *)
     if (typarams_size typarams) = 0 then
       let (argument, env) = strip_and_mono env argument in
       let (env, methods) = monomorphize_methods env methods in
       let decl = MConcreteInstance (decl_id, name, argument, methods) in
       (Some decl, env)
     else
       (None, env)

and monomorphize_slot (env: env) (slot: typed_slot): (env * mono_slot) =
  let (TypedSlot (name, ty)) = slot in
  let (ty, env) = strip_and_mono env ty in
  (env, MonoSlot (name, ty))

and monomorphize_slots (env: env) (slots: typed_slot list): (env * mono_slot list) =
  Util.map_with_context (fun (e, s) -> monomorphize_slot e s) env slots

and monomorphize_case (env: env) (case: linked_case): (env * mono_case) =
  let (LCase (_, name, slots)) = case in
  let (env, slots) = monomorphize_slots env slots in
  (env, MonoCase (name, slots))

and monomorphize_param (env: env) (param: value_parameter): (env * mvalue_parameter) =
  let (ValueParameter (name, ty)) = param in
  let (ty, env) = strip_and_mono env ty in
  (env, MValueParameter (name, ty))

and monomorphize_params (env: env) (params: value_parameter list): (env * mvalue_parameter list) =
  Util.map_with_context (fun (e, p) -> monomorphize_param e p) env params

and monomorphize_methods (env: env) (methods: typed_method_def list): (env * concrete_method list) =
  Util.map_with_context (fun (e, m) -> monomorphize_method e m) env methods

and monomorphize_method (env: env) (meth: typed_method_def): (env * concrete_method) =
  let (TypedMethodDef (id, name, params, rt, body)) = meth in
  let (env, params) = monomorphize_params env params in
  let (rt, env) = strip_and_mono env rt in
  let (body, env) = monomorphize_stmt env body in
  (env, MConcreteMethod (id, name, params, rt, body))

(* Monomorphize modules *)

open Outcome.Syntax

let rec monomorphize (env: env) (m: LinearityCheck.checked_module): (env * mono_module) Outcome.t =
  (* Monomorphize what we can: concrete definitions. *)
  let (TypedModule (module_name, decls)) = LinearityCheck.as_typed_module m in
  let* (env, declopts) =
    Outcome.traverse_with_context
      (fun (e, d) -> Outcome.catch (fun () -> let (d, e) = monomorphize_decl e d in (e, d)))
      env
      decls
  in
  let decls: mdecl list = List.filter_map (fun x -> x) declopts in
  (* Recursively collect and instantiate monomorphs until everything's instantiated. *)
  let* (env, decls') = Outcome.catch (fun () -> instantiate_monomorphs_until_exhausted env) in
  Ok (env, MonoModule (module_name, decls @ decls'))

and instantiate_monomorphs_until_exhausted (env: env): (env * mdecl list) =
  (* Get uninstantiated monomorphs from the environment. *)
  let monos: monomorph list = get_uninstantiated_monomorphs env in
  match monos with
  | first::rest ->
     (* If there are uninstantiated monomorphs, instantite them, and repeat the
        process. *)
     let (env, decls): (env * mdecl list) = instantiate_monomorphs env (first::rest) in
     let (env, decls') : (env * mdecl list) = instantiate_monomorphs_until_exhausted env in
     (env, decls @ decls')
  | [] ->
     (* If there are no uninstantiated monomorphs, we're done. *)
     (env, [])

and instantiate_monomorphs (env: env) (monos: monomorph list): (env * mdecl list) =
  (* Instantiate a list of monomorphs. *)
  Util.map_with_context (fun (e, m) -> instantiate_monomorph e m) env monos

and instantiate_monomorph (env: env) (mono: monomorph): (env * mdecl) =
  match mono with
  | MonoRecordDefinition { id; type_id; tyargs; _ } ->
     ((* Find the record definition and extract the type parameters and the slot
         list. *)
      let { typarams; slots; _ }: record_decl = get_record env type_id in
      (* Search/replace the type variables in the slot list with the type
         arguments from this monomorph. *)
      let slots: typed_slot list =
        List.map
          (fun (TypedSlot (name, ty)) ->
            (* TODO: extract decl name, make it a qname, pass that here *)
            TypedSlot (name, replace_type_variables typarams tyargs ty))
          slots
      in
      (* Strip and monomorphize the slot list. *)
      let (env, slots): (env * mono_slot list) = monomorphize_slot_list env slots in
      (* Store the monomorphic slot list in the environment. *)
      let env = store_record_monomorph_definition env id slots in
      (* Construct a monomorphic record decl. *)
      let decl: mdecl = MRecordMonomorph (id, slots) in
      (* Return the new environment and the declaration. *)
      (env, decl))
  | MonoUnionDefinition { id; type_id; tyargs; _ } ->
     (* Find the list of type parameters from the union definition. *)
     let { typarams; _ }: union_decl = get_union NoSpan env type_id in
     (* Find the list of cases from the env. *)
     let cases: typed_case list = List.map union_case_to_typed_case (get_union_cases env type_id) in
     (* Search/replace the type variables in the case list with the type
        arguments from this monomorph. *)
     let cases: typed_case list =
       List.map
         (fun (TypedCase (name, slots)) ->
           let slots: typed_slot list =
             List.map
               (fun (TypedSlot (name, ty)) ->
                 TypedSlot (name, replace_type_variables typarams tyargs ty))
               slots
           in
           TypedCase (name, slots))
         cases
     in
     (* Strip and monomorphize the case list. *)
     let (env, cases): (env * mono_case list) = monomorphize_case_list env cases in
     (* Store the monomorphic slot list in the environment. *)
     let env = store_union_monomorph_definition env id cases in
     (* Construct a monomorphic record decl. *)
     let decl: mdecl = MUnionMonomorph (id, cases) in
     (* Return the new environment and the declaration. *)
     (env, decl)
  | MonoFunction { id; function_id; tyargs; _ } ->
     (* Find the function's type parameters, value parameters, return type, and
        body. *)
     let (typarams, params, rt, body) = get_function_definition env function_id in
     (* Search/replace the type variables in the parameter list with the type
        arguments. *)
     let params: value_parameter list =
       List.map
         (fun (ValueParameter (name, ty)) ->
           ValueParameter (name, replace_type_variables typarams tyargs ty))
         params
     in
     (* Monomorphize the parameter list. *)
     let (env, params) = monomorphize_params env params in
     (* Search/replace the type variables in the return type. *)
     let rt: ty = replace_type_variables typarams tyargs rt in
     (* Monomorphize the return type. *)
     let (rt, env) = strip_and_mono env rt in
     (* Search/replace the type variables in the body with the type
        arguments. *)
     let bindings = make_bindings typarams tyargs in
     let body = replace_tyvars_stmt bindings body in
     (* Monomorphize the body *)
     let (body, env) = monomorphize_stmt env body in
     (* Store the monomorphic body in the environment. *)
     let env = store_function_monomorph_definition env id body in
     (* Construct a monomorphic record decl. *)
     let decl: mdecl = MFunctionMonomorph (id, function_id, params, rt, body) in
     (* Return the new environment and the declaration. *)
     (env, decl)
  | MonoInstanceMethod { id; method_id; tyargs; _ } ->
     ((* Find the methods's type parameters, value parameters, return type, and
         body. *)
      let (typarams, params, rt, body) = get_method_definition env method_id in
      (* Search/replace the type variables in the parameter list with the type
         arguments. *)
      let params: value_parameter list =
        List.map
          (fun (ValueParameter (name, ty)) ->
            ValueParameter (name, replace_type_variables typarams tyargs ty))
          params
      in
      (* Monomorphize the parameter list. *)
      let (env, params) = monomorphize_params env params in
      (* Search/replace the type variables in the return type. *)
      let rt = replace_type_variables typarams tyargs rt in
      (* Monomorphize the return type. *)
      let (rt, env) = strip_and_mono env rt in
      (* Search/replace the type variables in the body with the type
         arguments. *)
      let bindings = make_bindings typarams tyargs in
      let body = replace_tyvars_stmt bindings body in
      (* Monomorphize the body *)
      let (body, env) = monomorphize_stmt env body in
      (* Store the monomorphic body in the environment. *)
      let env = store_instance_method_monomorph_definition env id body in
      (* Construct a monomorphic record decl. *)
      let decl: mdecl = MMethodMonomorph (id, params, rt, body) in
      (* Return the new environment and the declaration. *)
      (env, decl))

(* Utils *)


and get_function_definition (env: env) (id: function_id): (typarams * value_parameter list * ty * tstmt) =
  let { name; typarams; value_params; rt; impl; _ }: function_decl = get_function env id in
  match impl with
  | Body body ->
     (typarams, value_params, rt, body)
  | BodyNotAttached | Foreign _ | ForeignGeneric ->
     (* Depends on the stage: the body extraction pass stores the body of every
        function written in Austral. A foreign function cannot have type
        parameters (030/055) or be exported, so no monomorph of one is made. *)
     internal_err NoSpan ("function `" ^ (ident_string name) ^ "` has no body")

and get_method_definition (env: env) (id: ins_meth_id): (typarams * value_parameter list * ty * tstmt) =
  let (InsMethRec { instance_id; value_params; rt; body; _ }) = get_instance_method env id in
  let { typarams; _ }: instance_decl = get_instance_decl NoSpan env instance_id in
  match body with
  | Some body ->
     (typarams, value_params, rt, body)
  | None ->
     (* Depends on the stage: the body extraction pass stores the body of every
        instance method. *)
     internal_err NoSpan ("method with ID `" ^ (show_instance_id instance_id) ^ "` has no body")

and monomorphize_slot_list (env: env) (slots: typed_slot list): (env * mono_slot list) =
  let names: identifier list = List.map (fun (TypedSlot (n, _)) -> n) slots in
  let (tys, env): (mono_ty list * env) = strip_and_mono_list env (List.map (fun (TypedSlot (_, t)) -> t) slots) in
  let (slots: mono_slot list) = List.map2 (fun name ty -> MonoSlot (name, ty)) names tys in
  (env, slots)

and monomorphize_case_list (env: env) (cases: typed_case list): (env * mono_case list) =
  Util.map_with_context
    (fun (env, TypedCase (name, slots)) ->
      let (env, slots) = monomorphize_slot_list env slots in
      (env, MonoCase (name, slots)))
    env
    cases

and replace_type_variables (typarams: typarams) (args: mono_type_bindings) (ty: ty): ty =
  let bindings: type_bindings = make_bindings typarams args in
  replace_variables bindings ty

and make_bindings (typarams: typarams) (args: mono_type_bindings): type_bindings =
  let typarams = typarams_as_list typarams in
  if (List.length typarams) = (List.length (mono_bindings_as_list args)) then
    let pairs: (type_parameter * ty) list =
      List.map (fun tp ->
          (match get_mono_binding args tp with
           | Some ty ->
              (tp, mono_to_ty ty)
           | None ->
              generic_error NoSpan ParameterNotFound))
        typarams
    in
    let b = bindings_from_list NoSpan pairs in
    b
  else
    generic_error NoSpan (ParameterArgumentLengthMismatch {
        parameters = typarams;
        arguments = List.map (fun (_, ty) -> ty) (mono_bindings_as_list args);
      })

and mono_to_ty (ty: mono_ty): ty =
  let r = mono_to_ty in
  match ty with
  | MonoUnit -> Unit
  | MonoBoolean -> Boolean
  | MonoInteger (s, w) -> Integer (s, w)
  | MonoSingleFloat -> SingleFloat
  | MonoDoubleFloat -> DoubleFloat
  | MonoNamedType mono_id ->
     (* SPECIAL CASE *)
     MonoTy mono_id
  | MonoRegionTy r ->
     RegionTy r
  | MonoReadRef (ty, region) ->
     ReadRef (r ty, ref_region_of_ty (r region))
  | MonoWriteRef (ty, region) ->
     WriteRef (r ty, ref_region_of_ty (r region))
  | MonoSpan (ty, region) ->
     Span (r ty, ref_region_of_ty (r region))
  | MonoSpanMut (ty, region) ->
     SpanMut (r ty, ref_region_of_ty (r region))
  | MonoAddress ty ->
     Address (r ty)
  | MonoPointer ty ->
     Pointer (r ty)
  | MonoFnPtr (args, rt) ->
     FnPtr (List.map r args, r rt)
  | MonoRegionTyVar (name, source) ->
     TyVar (TypeVariable (name, RegionUniverse, source, []))

let monomorphize_wrappers (env: env): (env * mdecl list * (string * mono_id * mono_ty foreign_signature) list) Outcome.t =
  Outcome.catch (fun () ->
      (* Register the monomorph of each exported function. *)
      let (env, exported): (env * (string * mono_id * mono_ty foreign_signature) list) =
        Util.map_with_context
          (fun (env, (function_id, export_name, signature)) ->
            let (env, mono_id) = add_or_get_function_monomorph env function_id empty_mono_bindings in
            let (signature, env) = monomorphize_foreign_signature env signature in
            (env, (export_name, mono_id_of_function mono_id, signature)))
          env
          (get_export_functions env)
      in
      (* Instantiate monomorphs *)
      let (env, decls): (env * mdecl list) = instantiate_monomorphs_until_exhausted env in
      (env, decls, exported))
