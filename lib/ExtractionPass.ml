(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Common
open Type
open TypeSystem
open Stages.SmallCombined
open Stages
open Linked
open Stages.AstDB
open Id
open InstanceIdSet
open Env
open EnvTypes
open Imports
open TypeParser
open TypeSignature
open TypeParameter
open TypeParameters
open TypeClasses
open RegionMap
open BuiltIn
open TypeVarSet
open Util
open Error
open ForeignType

let check_slots_are_free (place: place) (name: identifier) (slots: typed_slot list): unit =
  let check_slot_is_free (slot: typed_slot): unit =
    let (TypedSlot (_, ty)) = slot in
    match type_universe ty with
    | FreeUniverse ->
       ()
    | RegionUniverse ->
       ()
    | LinearUniverse ->
        linearity_error place (FreeContainsLinear { name; universe = LinearUniverse; parameter = false })
    | TypeUniverse ->
        linearity_error place (FreeContainsLinear { name; universe = TypeUniverse; parameter = false })
  in
  List.iter check_slot_is_free slots

let check_cases_are_free (place: place) (cases: typed_case list): unit =
  let check_case_is_free (case: typed_case): unit =
    let (TypedCase (name, slots)) = case in
    check_slots_are_free place name slots
  in
  List.iter check_case_is_free cases

let check_typarams_are_free (place: place) (name: identifier) (typarams: typarams): unit =
  let check_universe (u: universe) =
    match u with
    | FreeUniverse ->
       ()
    | RegionUniverse ->
       ()
    | LinearUniverse ->
       linearity_error place (FreeContainsLinear { name; universe = LinearUniverse; parameter = true })
    | TypeUniverse ->
       linearity_error place (FreeContainsLinear { name; universe = TypeUniverse; parameter = true })
  in
  List.iter (fun tp -> check_universe (typaram_universe tp)) (typarams_as_list typarams)

let check_all_type_parameters_appear_in_signature (place: place) (typarams: typarams) (params: value_parameter list) (rt: ty): unit =
  (* Get the type variables from the value parameter list. *)
  let param_tyvars: TypeVarSet.t list = List.map (fun (ValueParameter (_, ty)) -> type_variables ty) params in
  let param_tyvars: TypeVarSet.t = List.fold_left TypeVarSet.union TypeVarSet.empty param_tyvars in
  (* Get the type variables from the return type. *)
  let rt_tyvars: TypeVarSet.t = type_variables rt in
  (* Merge them to get the type variables in the signature. *)
  let signature_tyvars: TypeVarSet.t = TypeVarSet.union param_tyvars rt_tyvars in
  (* Go through each of the type parameters, and assert that it is in `singature_tyvars`. *)
  let check_typaram (tp: type_parameter): unit =
    let tv: type_var = typaram_to_tyvar tp in
    if TypeVarSet.mem tv signature_tyvars then
      ()
    else
      declaration_error place (TyparamNotInSignature tp)
  in
  List.iter check_typaram (typarams_as_list typarams)

(* What is left of extracting a definition once the IDs of the records and
   unions of its module are reserved: it gets the environment and the type
   signatures of the module, and registers the definition. *)
type extraction = env -> type_signature list -> env * linked_definition

(* The parsers of the types a definition writes, which look the types of the
   module up in its signatures. *)
let parsers (env: env) (local_types: type_signature list)
    : (place -> region_map -> typarams -> Ast.qtypespec -> ty)
      * (place -> typarams -> Combined.qslot -> typed_slot)
      * (place -> typarams -> Combined.qparam -> value_parameter) =
  let parse' (place: place) = parse_type place env local_types in
  let parse_slot (place: place) (typarams: typarams) (Combined.QualifiedSlot (n, ts)): typed_slot =
    TypedSlot (n, parse' place empty_region_map typarams ts)
  and parse_param (place: place) (typarams: typarams) (Combined.QualifiedParameter (n, ts)): value_parameter =
    ValueParameter (n, parse' place empty_region_map typarams ts)
  in
  (parse', parse_slot, parse_param)

(* The first region type variable that a value of this type holds, in the C
   the code generator writes for it. A reference, a pointer and an address hold
   a value of their first type; a record or a union holds its slots; a span and
   a function pointer are one C type whatever their types are. *)
let rec region_held_by_value (place: place) (env: env) (seen: ty list) (ty: ty): identifier option =
  match ty with
  | TyVar (TypeVariable (name, universe, _, _)) ->
     (match universe with
      | RegionUniverse -> Some name
      | FreeUniverse | LinearUniverse | TypeUniverse -> None)
  | ReadRef (ty, _) | WriteRef (ty, _) | Address ty | Pointer ty ->
     region_held_by_value place env seen ty
  | NamedType (_, id, args, _) ->
     if List.exists (equal_ty ty) seen then
       None
     else
       let seen = ty :: seen in
       let in_slots (typarams: typarams) (slots: typed_slot list): identifier option =
         let bindings = TypeBindings.bindings_from_list place (List.combine (typarams_as_list typarams) args) in
         List.find_map
           (fun (TypedSlot (_, slot_ty)) -> region_held_by_value place env seen (TypeBindings.replace_variables bindings slot_ty))
           slots
       in
       (match id with
        | RecordTypeId id ->
           let { typarams; slots; _ }: record_decl = get_record env id in
           in_slots typarams slots
        | UnionTypeId id ->
           let { typarams; _ }: union_decl = get_union place env id in
           List.find_map (fun ({ slots; _ }: union_case_decl) -> in_slots typarams slots) (get_union_cases env id))
  | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _
  | Span _ | SpanMut _ | FnPtr _ ->
     None

let definition_span (def: combined_definition): Span.span =
  match def with
  | CConstant (span, _, _, _, _, _) | CRecord (span, _, _, _, _, _, _) | CUnion (span, _, _, _, _, _, _)
  | CFunction (span, _, _, _, _, _, _, _, _) | CTypeclass (span, _, _, _, _, _) | CInstance (span, _, _, _, _, _, _, _) ->
     span

(* The methods of an instance without type parameters are compiled as they are
   written (Monomorphize, TInstance): nothing binds their own region
   parameters, so a value of such a type would reach the code generator, which
   has no C type for a region. A generic function, or a generic instance, is
   compiled only for the types of a call. Runs after every definition of the
   module is extracted, because a slot's type may be declared after the
   instance. *)
let check_instance_methods_hold_no_region (env: env) (def: combined_definition) (ldef: linked_definition): unit =
  let place: place = AtSpan (definition_span def) in
  match ldef with
  | LInstance (_, _, _, typarams, _, methods, _) ->
     if typarams_size typarams = 0 then
       let check (ty: ty): unit =
         match region_held_by_value place env [] ty with
         | Some name ->
            declaration_error place (RegionParameterUsedAsType name)
         | None ->
            ()
       in
       List.iter
         (fun (LMethodDef (_, _, params, rt, _, _)) ->
           List.iter (fun (ValueParameter (_, ty)) -> check ty) params;
           check rt)
         methods
     else
       ()
  | LConstant _ | LRecord _ | LUnion _ | LFunction _ | LTypeclass _ ->
     ()

open Outcome.Syntax

let rec extract (env: env) (cmodule: combined_module): (env * linked_module) Outcome.t =
  let (CombinedModule {
           name;
           kind;
           interface_docstring;
           interface_imports;
           body_docstring;
           body_imports;
           decls;
           _
      }) = cmodule in
  let* (env, mod_id) =
    Outcome.catch (fun () ->
        (* Add the module to the environment. *)
        let imported_instances: InstanceIdSet.t =
          InstanceIdSet.union
            (InstanceIdSet.of_list (imported_instances interface_imports))
            (InstanceIdSet.of_list (imported_instances body_imports))
        in
        let input: mod_input = {
            name;
            imported_instances;
          }
        in
        add_module env input)
  in
  (* Reserve the IDs of the records and unions of the module, and extract the
     list of local type signatures. This lets us parse type specifiers before
     the types they name are registered. *)
  let* (env, reserved): (env * (type_signature option * extraction) list) =
    Outcome.catch (fun () -> map_with_context (fun (env, def) -> reserve_definition env mod_id name def) env decls)
  in
  let sigs: type_signature list = List.filter_map fst reserved in
  (* Extract all declarations from the module into the environment. *)
  let* (env, linked_defs) = extract_definitions env sigs (List.map snd reserved) in
  let* () = Outcome.catch (fun () -> List.iter2 (check_instance_methods_hold_no_region env) decls linked_defs) in
  (* Construct the linked module *)
  let lmod = LinkedModule {
                 mod_id = mod_id;
                 name = name;
                 kind = kind;
                 interface_docstring = interface_docstring;
                 interface_imports = interface_imports;
                 body_docstring = body_docstring;
                 body_imports = body_imports;
                 decls = linked_defs
               }
  in
  Ok (env, lmod)

(** Given the environment, the list of local type signatures, and what is left
    of extracting each definition of a module, add all relevant decls to the
    environment, and return all corresponding linked decls. *)
and extract_definitions (env: env) (local_types: type_signature list) (extractions: extraction list): (env * (linked_definition list)) Outcome.t =
  let f ((env, extraction): (env * extraction)): (env * linked_definition) Outcome.t =
    Outcome.catch (fun () -> extraction env local_types)
  in
  Outcome.traverse_with_context f env extractions

(** Given the environment, the ID of the module we're extracting, and a
    combined definition, reserve the ID of a record or a union, and return its
    type signature, with what is left of extracting the definition: adding all
    relevant decls to the environment, and returning a linked definition. *)
and reserve_definition (env: env) (mod_id: mod_id) (mn: module_name) (def: combined_definition): (env * (type_signature option * extraction)) =
  let rm = empty_region_map in
  match def with
  | CConstant (span, vis, name, typespec, def, docstring) ->
     (env, (None, fun env local_types ->
     let (parse', _, _) = parsers env local_types in
     (let place: place = AtSpan span in
         let ty = parse' place rm empty_typarams typespec in
         let (env, decl_id) = register place env (NewConstant { mod_id; vis; name; ty; docstring }) in
         let decl = LConstant (decl_id, vis, name, ty, def, docstring) in
         (env, decl))))
  | CRecord (span, vis, name, typarams, universe, slots, docstring) ->
     let (env, id) = reserve_record_id env in
     (env, (Some (TypeSignature (name, typarams, universe, RecordTypeId id)), fun env local_types ->
     let (_, parse_slot, _) = parsers env local_types in
     (let place: place = AtSpan span in
         let slots = List.map (parse_slot place typarams) slots in
         (* If the universe is `Free`, check that there are no slots with types in the `Type` or `Linear` universes. *)
         let _ =
           match universe with
           | FreeUniverse ->
              check_slots_are_free place name slots
           | LinearUniverse | TypeUniverse | RegionUniverse ->
              ()
         in
         (* If the universe is `Free`, check that there are no type parameters in
            the `Linear` or `Type` universes. *)
         let _ =
           match universe with
           | FreeUniverse ->
              (* Unless it is `Address` or `Pointer` *)
              let qname = make_qident (mn, name, name) in
              if (is_address_type qname) || (is_pointer_type qname) then
                ()
              else
                check_typarams_are_free place name typarams
           | LinearUniverse | TypeUniverse | RegionUniverse ->
              ()
         in
         (* If the universe is `Region`, fail. *)
         let _ =
           match universe with
           | RegionUniverse ->
              declaration_error place (TypesAreNotRegions { name; declaration = RecordDeclaration })
           | FreeUniverse | LinearUniverse | TypeUniverse ->
              ()
         in
         let (env, decl_id) = register place env (NewRecord { id; mod_id; vis; name; docstring; typarams; universe; slots }) in
         let decl = LRecord (decl_id, vis, name, typarams, universe, slots, docstring) in
         (env, decl))))
  | CUnion (span, vis, name, typarams, universe, cases, docstring) ->
     let (env, id) = reserve_union_id env in
     (env, (Some (TypeSignature (name, typarams, universe, UnionTypeId id)), fun env local_types ->
     let (_, parse_slot, _) = parsers env local_types in
     (let place: place = AtSpan span in
         (* If the universe is `Free`, check that there are no cases with slots with types in the `Type` or `Linear` universes. *)
         let _ =
           match universe with
           | FreeUniverse ->
              let cases = List.map (fun (Combined.QualifiedCase (n, slots)) -> TypedCase (n, List.map (parse_slot place typarams) slots)) cases
              in
              check_cases_are_free place cases
           | LinearUniverse | TypeUniverse | RegionUniverse ->
              ()
         in
         (* If the universe is `Free`, check that there are no type parameters in
            the `Linear` or `Type` universes. *)
         let _ =
           match universe with
           | FreeUniverse ->
              check_typarams_are_free place name typarams
           | LinearUniverse | TypeUniverse | RegionUniverse ->
              ()
         in
         (* If the universe is `Region`, fail. *)
         let _ =
           match universe with
           | RegionUniverse ->
              declaration_error place (TypesAreNotRegions { name; declaration = UnionDeclaration })
           | FreeUniverse | LinearUniverse | TypeUniverse ->
              ()
         in
         (* Add the union itself to the env *)
         let (env, union_id) = register place env (NewUnion { id; mod_id; vis; name; docstring; typarams; universe }) in
         (* If the union is public, the cases are public, otherwise they are private. *)
         let case_vis: vis =
           match vis with
           | TypeVisPublic -> VisPublic
           | TypeVisOpaque -> VisPrivate
           | TypeVisPrivate -> VisPrivate
         in
         (* Convert the list of cases into a list of union_case_input records *)
         let case_map (Combined.QualifiedCase (n, slots)): union_case_input =
           let docstring = Docstring "" in (* TODO: Store docstrings in slots and cases *)
           {
             mod_id = mod_id;
             vis = case_vis;
             union_id = union_id;
             name = n;
             slots = List.map (parse_slot place typarams) slots;
             docstring = docstring;
           }
         in
         let cases: union_case_input list = List.map case_map cases in
         (* Add the union cases to the environmenmt *)
         let (env, linked_cases) = add_union_cases place env cases in
         (* Construct the decl *)
         let decl = LUnion (union_id, vis, name, typarams, universe, linked_cases, docstring) in
         (env, decl))))
  | CFunction (span, vis, name, typarams, params, rt, body, docstring, pragmas) ->
     (env, (None, fun env local_types ->
     let (parse', _, parse_param) = parsers env local_types in
     (let place: place = AtSpan span in
         let value_params = List.map (parse_param place typarams) params
         and rt = parse' place rm typarams rt
         and external_name: string option =
           (match pragmas with
            | [ForeignImportPragma s] ->
               Some s
            | [] | [ForeignExportPragma _] | [UnsafeModulePragma] | _ :: _ :: _ ->
               None)
         and export_name: string option =
           (match pragmas with
            | [ForeignExportPragma name] ->
               Some name
            | [] | [ForeignImportPragma _] | [UnsafeModulePragma] | _ :: _ :: _ ->
               None)
         in
         let _ =
           (* Check: a function is imported or exported, not both. The two
              names above are read only from a list of one pragma, so this
              looks at the list itself. *)
           let is_import = function
             | ForeignImportPragma _ -> true
             | ForeignExportPragma _ | UnsafeModulePragma -> false
           and is_export = function
             | ForeignExportPragma _ -> true
             | ForeignImportPragma _ | UnsafeModulePragma -> false
           in
           if (List.exists is_import pragmas) && (List.exists is_export pragmas) then
             declaration_error place (ForeignImportExport name)
           else
             ()
         in
         let _ =
           (* Check: if we have an export name the function can't be generic *)
           match export_name with
           | Some _ ->
              if typarams_size typarams > 0 then
                declaration_error place (ExportGeneric name)
              else
                ()
           | None ->
              ()
         in
         let _ =
           (* Check: all type parameters must appear in the type signature. *)
           check_all_type_parameters_appear_in_signature place typarams value_params rt
         in
         (* A taken name is reported before a signature that is not allowed, as
            it was when the signature was checked in a later stage. *)
         name_must_be_free place env mod_id name;
         let fn_input: function_input = {
             mod_id = mod_id;
             vis = vis;
             name = name;
             docstring = docstring;
             typarams = typarams;
             value_params = value_params;
             rt = rt;
             impl = (match external_name with
                     | Some s ->
                        if typarams_size typarams = 0 then
                          Foreign (s, check_signature place Import value_params rt)
                        else
                          ForeignGeneric
                     | None -> BodyNotAttached)
           }
         in
         let export: (string * ty foreign_signature) option =
           Option.map (fun name -> (name, check_signature place Export value_params rt)) export_name
         in
         let (env, decl_id) = register place env (NewFunction fn_input) in
         let env =
           (* If the function has an export name, register it in the environment. *)
           match export with
           | Some (name, signature) ->
              add_exported_function env decl_id name signature
           | None ->
              env
         in
         let decl = LFunction (decl_id, vis, name, typarams, value_params, rt, body, docstring, pragmas) in
         (env, decl))))
  | CTypeclass (span, vis, name, typaram, methods, docstring) ->
     (env, (None, fun env local_types ->
     let (parse', _, parse_param) = parsers env local_types in
     (let place: place = AtSpan span in
         (* Check: the universe is one of {Type, Linear, Free} *)
         let _ =
           match typaram_universe typaram with
           | TypeUniverse -> ()
           | LinearUniverse -> ()
           | FreeUniverse -> ()
           | RegionUniverse ->
              declaration_error place (TypeclassParamIsRegion name)
         in
         (* Add the typeclass itself to the env *)
         let (env, typeclass_id) = register place env (NewTypeClass { mod_id; vis; name; docstring; param = typaram; }) in
         (* Convert the list of methods into a list of type_class_method records *)
         let typarams = typarams_from_list place [typaram] in
         let method_map (Combined.CMethodDecl (name, method_typarams, params, rt, docstring)): type_class_method_input =
           let effective_typarams: typarams = merge_typarams place typarams method_typarams in
           let value_params = List.map (parse_param place effective_typarams) params
           and rt = parse' place rm effective_typarams rt in
           {
             mod_id;
             vis;
             typeclass_id;
             name;
             docstring;
             typarams = method_typarams;
             value_params;
             rt;
           }
         in
         let methods: type_class_method_input list = List.map method_map methods in
         (* Add the methods to the env *)
         let (env, linked_methods) = add_type_class_methods place env methods in
         (* Construct the decl *)
         let decl = LTypeclass (typeclass_id, vis, name, typaram, linked_methods, docstring) in
         (env, decl))))
  | CInstance (span, decl_span, vis, name, typarams, argument, methods, docstring) ->
     (env, (None, fun env local_types ->
     let (parse', _, parse_param) = parsers env local_types in
     (let place: place = AtSpan span in
         let typeclass_name = qident_to_sident name |> sident_name in
         (* First add the instance to the env, then the methods. *)
         let argument = parse' place rm typarams argument in
         (* Find typeclass info. *)
         let (typeclass_id, typeclass_mod_id, typeclass_param_name, universe): typeclass_id * mod_id * identifier * universe =
           match get_decl_by_name place env (qident_to_sident name) with
           | Some decl ->
              (match decl with
               | TypeClass { id; mod_id; param; _ } ->
                  (id, mod_id, typaram_name param, typaram_universe param)
               | Constant _ | Record _ | Union _ | UnionCase _ | Function _
               | TypeClassMethod _ | Instance _ ->
                  declaration_error place (InstanceForNonTypeclass typeclass_name))
           | None ->
              declaration_error place (UnknownTypeclass typeclass_name)
         in
         (* Check the argument has the right universe for the typeclass. *)
         let _ = check_instance_argument_has_right_universe place universe argument in
         (* Check the argument has the right shape. *)
         let _ = check_instance_argument_has_right_shape place typarams argument in
         (* Check that the non of the type parameters in the generic instance
            collide with the type parameter of the typeclass. *)
         let _ = check_disjoint_typarams place typeclass_param_name typarams in
         (* Local uniqueness: does this instance collide with other instances in this module? *)
         let _ =
           let other_instances: instance_decl list =
             List.filter (fun (instance: instance_decl) -> equal_typeclass_id typeclass_id instance.typeclass_id)
               (module_instances env mod_id)
           in
           check_instance_locally_unique place other_instances argument
         in
         (* Global uniqueness: check orphan rules. *)
         let _ = check_instance_orphan_rules place (AtSpan decl_span) env mod_id typeclass_mod_id argument in
         (* Add the instance to the env *)
         let input: instance_input = { mod_id; vis; typeclass_id; docstring; typarams; argument } in
         let (env, instance_id) = register place env (NewInstance input) in
         (* Convert the list of methods into a list of instance_method_input records *)
         let method_map (CMethodDef (name, method_typarams, params, rt, meth_docstring, body)): (instance_method_input * astmt) =
           let effective_typarams: typarams = merge_typarams place typarams method_typarams in
           let value_params = List.map (parse_param place effective_typarams) params
           and rt = parse' place rm effective_typarams rt
           and method_id: typeclass_method_id =
             (match get_method_from_typeclass_id_and_name env typeclass_id name with
              | Some { id; _ } ->
                 id
              | None ->
                 declaration_error place (TypeclassNoMethod {
                     typeclass = typeclass_name;
                     method_name = name;
                   }))
           in
           ({
               instance_id = instance_id;
               method_id = method_id;
               docstring = meth_docstring;
               name = name;
               typarams = method_typarams;
               value_params = value_params;
               rt = rt;
               body = None;
             },
            body)
         in
         let methods: (instance_method_input * astmt) list = List.map method_map methods in
         (* Add the methods to the env *)
         let (env, linked_methods) = add_instance_methods place env methods in
         (* Construct the decl *)
         let decl = LInstance (instance_id, vis, name, typarams, argument, linked_methods, docstring) in
         (env, decl))))

(** Utility functions to add lists of things to the environment. *)
and add_union_cases (place: place) (env: env) (cases: union_case_input list): (env * (linked_case list)) =
  let f ((env, input): (env * union_case_input)): (env * linked_case) =
    let (env, decl_id) = register place env (NewUnionCase input) in
    let { name; slots; _ } = input in
    let case = LCase (decl_id, name, slots) in
    (env, case)
  in
  map_with_context f env cases

and add_type_class_methods (place: place) (env: env) (cases: type_class_method_input list): (env * (linked_method_decl list)) =
  let f ((env, input): (env * type_class_method_input)): (env * linked_method_decl) =
    let (env, decl_id) = register place env (NewTypeClassMethod input) in
    let { name; value_params; rt; docstring; vis; _ } = input in
    (* LOAD-BEARING HACK: need to destructure `vis` because otherwise the
       compiler thinks `input` is supposed to be an instance of
       `instance_method_input` (???). *)
    let _ = vis in
    let decl = LMethodDecl (decl_id, name, value_params, rt, docstring) in
    (env, decl)
  in
  map_with_context f env cases

and add_instance_methods (place: place) (env: env) (cases: (instance_method_input * astmt) list): (env * (linked_method_def list)) =
  let f ((env, pair): (env * (instance_method_input * astmt))): (env * linked_method_def) =
    let (input, body) = pair in
    let (env, decl_id) = register place env (NewInstanceMethod input) in
    let { name; value_params; rt; docstring; _ } = input in
    let decl = LMethodDef (decl_id, name, value_params, rt, docstring, body) in
    (env, decl)
  in
  map_with_context f env cases
