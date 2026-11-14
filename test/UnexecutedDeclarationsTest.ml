(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(* What the environment, the declaration stages and the late stages do with
   values no program gives them: an ID from a later environment, a declaration
   registered twice, an instance argument that extraction rejects first, a
   monomorph whose declaration has no body, a node the earlier stages never
   build. Each test builds the value by hand. *)
open OUnit2
open Austral_core
open Austral_core.Identifier
open Austral_core.Common
open Austral_core.Type
open Austral_core.EnvTypes
open Austral_core.Error
open Austral_core.MonoType
open Austral_core.Stages.Mtast
module InstanceIdSet = Austral_core.InstanceIdSet.InstanceIdSet

let error_of (f: unit -> 'a): error option =
  match f () with
  | _ -> None
  | exception Austral_error (AustralError { error; _ }) -> Some error

let outcome_error (outcome: 'a Outcome.t): error option =
  match outcome with
  | Ok _ -> None
  | Error (AustralError { error; _ }, _) -> Some error

let internal (message: string): error option =
  Some (Internal message)

let example: module_name = make_mod_name "Example"

let named (name: string): qident =
  make_qident (example, make_ident name, make_ident name)

let docstring: docstring = Docstring ""

let span: Span.span = Span.empty_span

let typaram (name: string) (source: string): TypeParameter.type_parameter =
  TypeParameter.make_typaram (make_ident name, FreeUniverse, named source, [])

let typarams (params: TypeParameter.type_parameter list): TypeParameters.typarams =
  TypeParameters.typarams_from_list NoSpan params

let tyvar (name: string) (universe: universe): ty =
  TyVar (TypeVariable (make_ident name, universe, named "f", []))

let add_example (env: Env.env): Env.env * Id.mod_id =
  Env.add_module env { name = example; imported_instances = InstanceIdSet.empty }

let register_function (env: Env.env) (mod_id: Id.mod_id) (name: string) (params: value_parameter list) (rt: ty) (impl: function_impl): Env.env * Id.function_id =
  Env.register NoSpan env (NewFunction {
      mod_id;
      vis = VisPublic;
      name = make_ident name;
      docstring;
      typarams = TypeParameters.empty_typarams;
      value_params = params;
      rt;
      impl
    })

let register_typeclass (env: Env.env) (mod_id: Id.mod_id): Env.env * Id.typeclass_id =
  Env.register NoSpan env (NewTypeClass {
      mod_id;
      vis = VisPublic;
      name = make_ident "C";
      docstring;
      param = TypeParameter.make_typaram (make_ident "T", TypeUniverse, named "C", [])
    })

(* The typeclass C with the method `m(): Unit`. *)
let register_typeclass_with_method (env: Env.env) (mod_id: Id.mod_id): Env.env * Id.typeclass_id * Id.typeclass_method_id =
  let (env, typeclass_id) = register_typeclass env mod_id in
  let (env, method_id) =
    Env.register NoSpan env (NewTypeClassMethod {
        mod_id;
        vis = VisPublic;
        typeclass_id;
        name = make_ident "m";
        docstring;
        typarams = TypeParameters.empty_typarams;
        value_params = [];
        rt = Unit
      })
  in
  (env, typeclass_id, method_id)

let register_instance (env: Env.env) (mod_id: Id.mod_id) (typeclass_id: Id.typeclass_id) (params: TypeParameters.typarams) (argument: ty): Env.env * Id.instance_id =
  Env.register NoSpan env (NewInstance {
      mod_id;
      vis = VisPublic;
      typeclass_id;
      docstring;
      typarams = params;
      argument
    })

let register_instance_method (env: Env.env) (instance_id: Id.instance_id) (method_id: Id.typeclass_method_id) (body: Stages.Tast.tstmt option): Env.env * Id.ins_meth_id =
  Env.register NoSpan env (NewInstanceMethod {
      instance_id;
      method_id;
      docstring;
      name = make_ident "m";
      typarams = TypeParameters.empty_typarams;
      value_params = [];
      rt = Unit;
      body
    })

(* The record `R[T]` with no slots. *)
let register_record (env: Env.env) (mod_id: Id.mod_id) (name: string): Env.env * Id.record_id =
  let (env, id) = Env.reserve_record_id env in
  Env.register NoSpan env (NewRecord {
      id;
      mod_id;
      vis = TypeVisPublic;
      name = make_ident name;
      docstring;
      typarams = typarams [typaram "T" name];
      universe = FreeUniverse;
      slots = []
    })

let foreign_signature (direction: ForeignType.direction) (params: value_parameter list) (rt: ty): ty ForeignType.foreign_signature =
  ForeignType.check_signature NoSpan direction params rt

let region: ty = RegionTy Region.static_region

(* One type of each kind, in the order of the constructors. *)
let one_of_each_kind (record_type_id: Id.type_decl_id): ty list =
  let ref_region = RefRegion Region.static_region in
  [
    Unit;
    Boolean;
    Integer (Signed, Width32);
    SingleFloat;
    DoubleFloat;
    NamedType (named "R", record_type_id, [], FreeUniverse);
    region;
    ReadRef (Unit, ref_region);
    WriteRef (Unit, ref_region);
    Span (Unit, ref_region);
    SpanMut (Unit, ref_region);
    tyvar "T" FreeUniverse;
    Address Unit;
    Pointer Unit;
    FnPtr ([], Unit)
  ]

(* Env: a lookup by ID misses when the ID is from a later environment. *)

let test_lookups_by_an_id_from_a_later_environment _ =
  let (before, mod_id) = add_example Env.empty_env in
  let (after, typeclass_id, method_id) = register_typeclass_with_method before mod_id in
  let (after, instance_id) = register_instance after mod_id typeclass_id TypeParameters.empty_typarams Unit in
  let (after, ins_meth_id) = register_instance_method after instance_id method_id None in
  let (after, record_id) = register_record after mod_id "R" in
  let (after, union_id) = Env.reserve_union_id after in
  let (after, union_id) =
    Env.register NoSpan after (NewUnion {
        id = union_id;
        mod_id;
        vis = TypeVisPublic;
        name = make_ident "U";
        docstring;
        typarams = TypeParameters.empty_typarams;
        universe = FreeUniverse
      })
  in
  let t = typaram "T" "R" in
  let (after, record_mono_id) =
    Env.add_or_get_record_monomorph after record_id (MonoTypeBindings.mono_bindings_from_list [(t, MonoUnit)])
  in
  let mono_id = Id.mono_id_of_record record_mono_id in
  let no (what: string): error option = internal ("No " ^ what ^ " in this environment.") in
  assert_equal (no "module with this ID")
    (error_of (fun () -> Env.module_name_from_id NoSpan Env.empty_env mod_id));
  assert_bool "the later environment has the declarations"
    (Id.equal_record_id record_id (Env.get_record after record_id).id);
  assert_equal (no ("typeclass with ID `" ^ (Id.show_typeclass_id typeclass_id) ^ "`"))
    (error_of (fun () -> Env.get_typeclass NoSpan before typeclass_id));
  assert_equal (no ("record with ID `" ^ (Id.show_record_id record_id) ^ "`"))
    (error_of (fun () -> Env.get_record before record_id));
  assert_equal (no ("union with ID `" ^ (Id.show_union_id union_id) ^ "`"))
    (error_of (fun () -> Env.get_union NoSpan before union_id));
  assert_equal (no ("instance with ID `" ^ (Id.show_instance_id instance_id) ^ "`"))
    (error_of (fun () -> Env.get_instance_decl NoSpan before instance_id));
  assert_equal (no ("instance method with ID `" ^ (Id.show_ins_meth_id ins_meth_id) ^ "`"))
    (error_of (fun () -> Env.get_instance_method before ins_meth_id));
  assert_equal (no ("monomorph with ID `" ^ (Id.show_mono_id mono_id) ^ "`"))
    (error_of (fun () -> Env.get_monomorph before mono_id));
  assert_equal (no ("record or union monomorph with ID `" ^ (Id.show_mono_id mono_id) ^ "`"))
    (error_of (fun () -> Env.get_type_monomorph before mono_id));
  assert_equal (no ("monomorph with ID `" ^ (Id.show_mono_id mono_id) ^ "`"))
    (error_of (fun () -> Env.store_record_monomorph_definition before record_mono_id []))

let test_a_reserved_id_registered_twice _ =
  let (env, mod_id) = add_example Env.empty_env in
  let (env, id) = Env.reserve_record_id env in
  let record (name: string): Id.record_id registration =
    NewRecord {
        id;
        mod_id;
        vis = TypeVisPublic;
        name = make_ident name;
        docstring;
        typarams = TypeParameters.empty_typarams;
        universe = FreeUniverse;
        slots = []
      }
  in
  let (env, _) = Env.register NoSpan env (record "A") in
  assert_equal (internal "A record or a union is already registered under this reserved ID.")
    (error_of (fun () -> Env.register NoSpan env (record "B")))

let test_a_function_body_stored_twice _ =
  let (env, mod_id) = add_example Env.empty_env in
  let impls: (string * function_impl) list = [
      ("body", Body (Stages.Tast.TSkip span));
      ("foreign", Foreign ("c_function", foreign_signature Import [] Boolean));
      ("foreign_generic", ForeignGeneric)
    ]
  in
  List.iter
    (fun (name, impl) ->
      let (env, id) = register_function env mod_id name [] Boolean impl in
      assert_equal ~msg:name (internal ("function `" ^ name ^ "` already has a body"))
        (error_of (fun () -> Env.store_function_body env id (Stages.Tast.TSkip span))))
    impls

let test_a_method_body_stored_twice _ =
  let (env, mod_id) = add_example Env.empty_env in
  let (env, typeclass_id, method_id) = register_typeclass_with_method env mod_id in
  let (env, instance_id) = register_instance env mod_id typeclass_id TypeParameters.empty_typarams Unit in
  let (env, ins_meth_id) = register_instance_method env instance_id method_id None in
  let env = Env.store_method_body env ins_meth_id (Stages.Tast.TSkip span) in
  assert_equal (internal "function `m` already has a body")
    (error_of (fun () -> Env.store_method_body env ins_meth_id (Stages.Tast.TSkip span)))

(* Only a constant is a variable. The type checker asks for a variable only
   after it found no function by that name. *)
let test_only_a_constant_is_a_variable _ =
  let (env, mod_id) = add_example Env.empty_env in
  let (env, _) = register_function env mod_id "f" [] Unit BodyNotAttached in
  let (env, _, _) = register_typeclass_with_method env mod_id in
  let (env, _) = register_record env mod_id "R" in
  let (env, union_id) = Env.reserve_union_id env in
  let (env, union_id) =
    Env.register NoSpan env (NewUnion {
        id = union_id;
        mod_id;
        vis = TypeVisPublic;
        name = make_ident "U";
        docstring;
        typarams = TypeParameters.empty_typarams;
        universe = FreeUniverse
      })
  in
  let (env, _) =
    Env.register NoSpan env (NewUnionCase { mod_id; vis = VisPublic; union_id; name = make_ident "K"; docstring; slots = [] })
  in
  let (env, _) =
    Env.register NoSpan env (NewConstant { mod_id; vis = VisPublic; name = make_ident "c"; ty = Boolean; docstring })
  in
  let variable (name: string) = Env.get_variable NoSpan env LexEnv.empty_lexenv (named name) in
  List.iter (fun name -> assert_equal ~msg:name None (variable name)) ["f"; "C"; "m"; "R"; "U"; "K"];
  assert_equal (Some (Boolean, LexEnv.VarConstant)) (variable "c")

let test_a_symbol_imported_twice _ =
  let imports = Imports.add_symbol (Imports.empty_map example) (named "f") in
  assert_equal (Some (Generic SymbolImportedTwice))
    (error_of (fun () -> Imports.add_symbol imports (named "f")))

(* Types the type parser never makes. *)

let test_signed_byte_size_and_index _ =
  List.iter
    (fun (signedness: signedness) ->
      assert_equal ~printer:Fun.id "ByteSize" (type_string (Integer (signedness, WidthByteSize)));
      assert_equal ~printer:Fun.id "Index" (type_string (Integer (signedness, WidthIndex))))
    [Unsigned; Signed]

let test_universes_of_arguments _ =
  List.iter
    (fun (universe: universe) ->
      assert_bool (universe_string universe ^ " is not Type")
        (not (TypeSystem.any_arg_is_type [tyvar "A" universe])))
    [FreeUniverse; LinearUniverse; RegionUniverse];
  List.iter
    (fun (universe: universe) ->
      assert_equal ~printer:show_universe universe (TypeBindings.replaced_universe universe [Unit]))
    [FreeUniverse; LinearUniverse; RegionUniverse]

let test_the_type_of_a_pointer_slot _ =
  let value = MPointerSlotAccessor (MLocalVar (make_ident "p", MonoPointer MonoUnit), make_ident "x", MonoBoolean) in
  assert_equal ~printer:show_mono_ty MonoBoolean (MtastUtil.get_type value)

(* Every kind of type, in both directions. Only a generic signature has a type
   variable, and a generic signature is not checked. *)
let test_types_across_the_c_boundary _ =
  let (_, record_id) = Env.reserve_record_id Env.empty_env in
  let allowed (direction: ForeignType.direction) (ty: ty): bool =
    match (direction, ty) with
    | (_, (Boolean | Integer _ | SingleFloat | DoubleFloat | FnPtr _)) -> true
    | (Import, (Span _ | SpanMut _ | Address _ | Pointer _)) -> true
    | (Export, Address Boolean) -> true
    | (Import, (Unit | NamedType _ | RegionTy _ | ReadRef _ | WriteRef _ | TyVar _))
    | (Export, (Unit | NamedType _ | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _
               | TyVar _ | Address _ | Pointer _)) -> false
  in
  let address_of_boolean (ty: ty): ty =
    match ty with
    | Address _ -> Address Boolean
    | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _ | ReadRef _
    | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Pointer _ | FnPtr _ -> ty
  in
  List.iter
    (fun (ty: ty) ->
      let ty = address_of_boolean ty in
      let params = [ValueParameter (make_ident "x", ty)] in
      let error_in (direction: ForeignType.direction) = error_of (fun () -> foreign_signature direction params Boolean) in
      assert_equal ~msg:("import " ^ type_string ty)
        (if allowed Import ty then None else Some (Generic ForeignFunctionTypeNotAllowed))
        (error_in Import);
      assert_equal ~msg:("export " ^ type_string ty)
        (if allowed Export ty then None else Some (Type (ExportDisallowedType ty)))
        (error_in Export))
    (one_of_each_kind (RecordTypeId record_id))

(* TypeClasses: instance arguments that extraction rejects before these
   checks. *)

let test_a_region_instance_argument _ =
  assert_equal (Some (Declaration InstanceArgumentRegion))
    (error_of (fun () -> TypeClasses.check_instance_argument_has_right_shape NoSpan TypeParameters.empty_typarams region))

let test_overlap_of_every_pair_of_kinds _ =
  let (env, mod_id) = add_example Env.empty_env in
  let (env, record_id) = Env.reserve_record_id env in
  let (env, typeclass_id) = register_typeclass env mod_id in
  let (env, instance_id) = register_instance env mod_id typeclass_id TypeParameters.empty_typarams Unit in
  let instance: instance_decl = Env.get_instance_decl NoSpan env instance_id in
  let kinds = List.mapi (fun i ty -> (i, ty)) (one_of_each_kind (RecordTypeId record_id)) in
  List.iter
    (fun (i, a) ->
      List.iter
        (fun (j, b) ->
          let msg = (type_string a) ^ " against an instance for " ^ (type_string b) in
          let expected: error option =
            match a with
            | RegionTy _ | TyVar _ ->
               Some (Generic InstanceOverlapTypeNotAllowed)
            | Pointer _ ->
               (* This fixes what the code does today, which issue #122 reports
                  as a bug: a pointer overlaps an instance for any type. When
                  #122 is fixed, a pointer belongs to the last arm. *)
               Some (Generic OverlappingInstances)
            | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
            | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | Address _ | FnPtr _ ->
               if i = j then Some (Generic OverlappingInstances) else None
          in
          assert_equal ~msg expected
            (error_of (fun () -> TypeClasses.check_instance_locally_unique NoSpan [{ instance with argument = b }] a)))
        kinds)
    kinds

let test_a_region_or_type_variable_is_local_to_no_module _ =
  let (env, mod_id) = add_example Env.empty_env in
  let (env, other_id) = Env.add_module env { name = make_mod_name "Other"; imported_instances = InstanceIdSet.empty } in
  List.iter
    (fun (ty: ty) ->
      assert_equal ~msg:(type_string ty) (Some (Generic BrokenOrphanRule))
        (error_of (fun () -> TypeClasses.check_instance_orphan_rules NoSpan NoSpan env mod_id other_id ty)))
    [region; tyvar "T" FreeUniverse]

let test_two_matching_instances_of_one_module _ =
  let (env, mod_id) = add_example Env.empty_env in
  let (env, typeclass_id) = register_typeclass env mod_id in
  let (env, _) = register_instance env mod_id typeclass_id TypeParameters.empty_typarams Unit in
  let (env, _) = register_instance env mod_id typeclass_id TypeParameters.empty_typarams Unit in
  assert_equal
    (Some (Type (OverlappingInstancesMatch { typeclass = make_ident "C"; ty = Unit; modules = [example] })))
    (error_of (fun () -> TypeMatch.get_instance NoSpan env example Unit typeclass_id))

(* Entrypoint: signatures whose types the type parser never makes. *)

let entrypoint_error (params: ty list) (rt: ty): error option =
  let (env, mod_id) = add_example Env.empty_env in
  let params = List.mapi (fun i ty -> ValueParameter (make_ident ("p" ^ string_of_int i), ty)) params in
  let (env, _) = register_function env mod_id "main" params rt BodyNotAttached in
  outcome_error (Entrypoint.entrypoint_code env (named "main"))

let test_entrypoint_types_of_no_declaration _ =
  let (_, record_id) = Env.reserve_record_id Env.empty_env in
  let named_in (universe: universe): ty =
    NamedType (named "R", RecordTypeId record_id, [], universe)
  in
  let types = one_of_each_kind (RecordTypeId record_id) @ List.map named_in [TypeUniverse; LinearUniverse; RegionUniverse] in
  List.iter
    (fun (ty: ty) ->
      assert_equal ~msg:(type_string ty) (Some (Entrypoint (WrongParameterType ty)))
        (entrypoint_error [ty] Unit);
      assert_equal ~msg:(type_string ty) (Some (Entrypoint (WrongReturnType ty)))
        (entrypoint_error [] ty))
    types

(* Monomorphize: monomorphs of declarations in a state the stages before it
   never leave them in. *)

let instantiate (env: Env.env): error option =
  error_of (fun () -> Monomorphize.instantiate_monomorphs_until_exhausted env)

let test_a_function_monomorph_without_a_body _ =
  let (env, mod_id) = add_example Env.empty_env in
  List.iter
    (fun (name, impl) ->
      let (env, id) = register_function env mod_id name [] Boolean impl in
      let (env, _) = Env.add_or_get_function_monomorph env id MonoTypeBindings.empty_mono_bindings in
      assert_equal ~msg:name (internal ("function `" ^ name ^ "` has no body")) (instantiate env))
    [
      ("not_attached", BodyNotAttached);
      ("foreign", Foreign ("c_function", foreign_signature Import [] Boolean));
      ("foreign_generic", ForeignGeneric)
    ]

let test_a_method_monomorph_without_a_body _ =
  let (env, mod_id) = add_example Env.empty_env in
  let (env, typeclass_id, method_id) = register_typeclass_with_method env mod_id in
  let (env, instance_id) = register_instance env mod_id typeclass_id TypeParameters.empty_typarams Unit in
  let (env, ins_meth_id) = register_instance_method env instance_id method_id None in
  let (env, _) = Env.add_or_get_instance_method_monomorph env ins_meth_id MonoTypeBindings.empty_mono_bindings in
  assert_equal (internal ("method with ID `" ^ (Id.show_instance_id instance_id) ^ "` has no body")) (instantiate env)

(* The type arguments of a monomorph are for other parameters than those of its
   declaration. *)
let test_a_monomorph_with_bindings_for_other_parameters _ =
  let (env, mod_id) = add_example Env.empty_env in
  let (env, record_id) = register_record env mod_id "R" in
  let other = typaram "U" "R" in
  let (env, _) = Env.add_or_get_record_monomorph env record_id (MonoTypeBindings.mono_bindings_from_list [(other, MonoUnit)]) in
  assert_equal (Some (Generic ParameterNotFound)) (instantiate env)

(* An instance whose type parameter its argument does not name: extraction
   rejects it (InstanceLeftoverTypeParameters), so no program builds this
   environment. A call of its method on `Unit`, the instance's argument, leaves
   the parameter out of the method's type arguments. The test executes the
   check that counts them and fixes the error it raises; it says nothing about
   a program. *)
let test_a_method_call_leaves_out_an_unbound_parameter _ =
  let (env, mod_id) = add_example Env.empty_env in
  let (env, typeclass_id, method_id) = register_typeclass_with_method env mod_id in
  let u = typaram "U" "C" in
  let (env, instance_id) = register_instance env mod_id typeclass_id (typarams [u]) Unit in
  let (env, _) = register_instance_method env instance_id method_id (Some (Stages.Tast.TSkip span)) in
  let call: Stages.Tast.texpr =
    TVarMethodCall {
        source_module_name = example;
        typeclass_id;
        params = [];
        method_name = named "m";
        args = [];
        dispatch_ty = Unit;
        rt = Unit;
        bindings = TypeBindings.empty_bindings
      }
  in
  let (env, function_id) = register_function env mod_id "g" [] Unit BodyNotAttached in
  let env = Env.store_function_body env function_id (TDiscarding (span, call)) in
  let (env, _) = Env.add_or_get_function_monomorph env function_id MonoTypeBindings.empty_mono_bindings in
  assert_equal (Some (Generic (ParameterArgumentLengthMismatch { parameters = [u]; arguments = [] })))
    (instantiate env)

(* CodeGen: monomorphic declarations the monomorphizer never makes. *)

let generate (env: Env.env) (decls: mdecl list): CRepr.c_decl list =
  match Env.with_supply env (fun supply -> CodeGen.gen_module env supply (MonoModule (example, decls))) with
  | Ok (_, CRepr.CUnit (_, decls)) -> decls
  | Error _ -> assert_failure "the module is generated"

let function_definitions (decls: CRepr.c_decl list): (CRepr.c_param list * CRepr.c_stmt) list =
  List.filter_map
    (fun (decl: CRepr.c_decl) ->
      match decl with
      | CFunctionDefinition (_, _, params, _, body) -> Some (params, body)
      | CMacro _ | CStructForwardDeclaration _ | CNamedStructDefinition _
      | CEnumDefinition _ | CFunctionDeclaration _ -> None)
    decls

let a_function (env: Env.env): Env.env * Id.function_id =
  let (env, mod_id) = add_example env in
  register_function env mod_id "f" [] Unit BodyNotAttached

let region_variable: mono_ty = MonoRegionTyVar (make_ident "R", named "f")

let test_parameters_of_region_types _ =
  let (env, id) = a_function Env.empty_env in
  let params = [
      MValueParameter (make_ident "r", MonoRegionTy Region.static_region);
      MValueParameter (make_ident "v", region_variable)
    ]
  in
  match function_definitions (generate env [MFunction (id, make_ident "f", params, MonoUnit, MSkip)]) with
  | [([CValueParam (_, r); CValueParam (_, v)], _)] ->
     assert_equal (CRepr.CBuiltinType CRegionType) r;
     assert_equal (CRepr.CBuiltinType CRegionType) v
  | _ ->
     assert_failure "one function with two parameters"

let test_borrows_of_values_that_are_not_addresses _ =
  let (env, id) = a_function Env.empty_env in
  List.iter
    (fun (orig_type: mono_ty) ->
      let borrow = MBorrow {
                       original = make_ident "x";
                       rename = make_ident "x_ref";
                       region = make_ident "R";
                       orig_type;
                       ref_type = MonoReadRef (orig_type, MonoRegionTy Region.static_region);
                       body = MSkip;
                       mode = Read
                     }
      in
      match function_definitions (generate env [MFunction (id, make_ident "f", [], MonoUnit, borrow)]) with
      | [(_, CBlock [CLet (_, CPointer _, Some (CAddressOf (CVar _))); CBlock []])] -> ()
      | _ -> assert_failure ("the borrow of a " ^ (show_mono_ty orig_type) ^ " takes its address"))
    [
      MonoSpan (MonoUnit, MonoRegionTy Region.static_region);
      MonoSpanMut (MonoUnit, MonoRegionTy Region.static_region);
      MonoPointer MonoUnit;
      MonoFnPtr ([], MonoUnit);
      region_variable
    ]

(* The wrapper of an imported function, with the signature of an exported
   one: an Address is a ForeignPointer for an import. *)
let test_an_imported_function_of_an_exported_signature _ =
  let (env, mod_id) = add_example Env.empty_env in
  let (env, exported_id) = register_function env mod_id "e" [] Unit BodyNotAttached in
  let (env, imported_id) = register_function env mod_id "i" [] Unit BodyNotAttached in
  let as_mono signature = fst (ForeignType.map_signature (fun () _ -> (MonoBoolean, ())) () signature) in
  let exported = foreign_signature Export [ValueParameter (make_ident "a", Address Boolean); ValueParameter (make_ident "f", FnPtr ([], Boolean))] Boolean
  and imported = foreign_signature Import [ValueParameter (make_ident "p", Pointer Boolean)] Boolean in
  let decls = [
      MForeignFunction (exported_id, make_ident "e", [
            MValueParameter (make_ident "a", MonoAddress MonoBoolean);
            MValueParameter (make_ident "f", MonoFnPtr ([], MonoBoolean))
          ], MonoBoolean, as_mono exported, "c_exported");
      MForeignFunction (imported_id, make_ident "i", [MValueParameter (make_ident "p", MonoPointer MonoBoolean)], MonoBoolean, as_mono imported, "c_imported")
    ]
  in
  match function_definitions (generate env decls) with
  | [(_, CBlock [_; CReturn (CFuncall (_, [CVar _; CVar _]))]); (_, CBlock [_; CReturn (CFuncall (_, [CVar _]))])] -> ()
  | _ -> assert_failure "the address, the function pointer and the pointer are passed as they are"

let kind (decl: CRepr.c_decl): string =
  match decl with
  | CMacro _ -> "macro"
  | CStructForwardDeclaration _ -> "forward"
  | CNamedStructDefinition _ -> "struct"
  | CEnumDefinition _ -> "enum"
  | CFunctionDeclaration _ -> "declaration"
  | CFunctionDefinition _ -> "definition"

(* A record monomorph before the declarations a module's own definitions
   generate: the sort compares its struct with each of them first. The
   pipeline never hands the declarations over in this order (the monomorphs of
   a module come after its own declarations); the test executes the
   comparisons of the sort that this order needs, and fixes their result. *)
let test_a_struct_before_other_declarations _ =
  let (env, mod_id) = add_example Env.empty_env in
  let (env, function_id) = register_function env mod_id "f" [] Unit BodyNotAttached in
  let (env, record_id) = register_record env mod_id "R" in
  let bindings (ty: mono_ty) = MonoTypeBindings.mono_bindings_from_list [(typaram "T" "R", ty)] in
  let (env, first) = Env.add_or_get_record_monomorph env record_id (bindings MonoUnit) in
  let (env, second) = Env.add_or_get_record_monomorph env record_id (bindings MonoBoolean) in
  let (env, union_id) = Env.reserve_union_id env in
  let (env, union_id) =
    Env.register NoSpan env (NewUnion {
        id = union_id;
        mod_id;
        vis = TypeVisPublic;
        name = make_ident "U";
        docstring;
        typarams = TypeParameters.empty_typarams;
        universe = FreeUniverse
      })
  in
  let (env, union_mono) = Env.add_or_get_union_monomorph env union_id MonoTypeBindings.empty_mono_bindings in
  let (env, constant_id) =
    Env.register NoSpan env (NewConstant { mod_id; vis = VisPublic; name = make_ident "c"; ty = Boolean; docstring })
  in
  let struct_then (decl: mdecl): string list =
    List.map kind (generate env [MRecordMonomorph (first, []); decl])
  in
  assert_equal ~printer:(String.concat " ") ["forward"; "struct"; "macro"]
    (struct_then (MConstant (constant_id, make_ident "c", MonoBoolean, MBoolConstant true)));
  assert_equal ~printer:(String.concat " ") ["forward"; "forward"; "struct"; "struct"]
    (struct_then (MRecordMonomorph (second, [])));
  assert_equal ~printer:(String.concat " ") ["enum"; "forward"; "forward"; "struct"; "struct"]
    (struct_then (MUnionMonomorph (union_mono, [])));
  assert_equal ~printer:(String.concat " ") ["forward"; "struct"; "declaration"; "definition"]
    (struct_then (MFunction (function_id, make_ident "f", [], MonoUnit, MSkip)))

let suite =
  "Unexecuted declarations" >::: [
      "lookups by an ID from a later environment" >:: test_lookups_by_an_id_from_a_later_environment;
      "a reserved ID registered twice" >:: test_a_reserved_id_registered_twice;
      "a function body stored twice" >:: test_a_function_body_stored_twice;
      "a method body stored twice" >:: test_a_method_body_stored_twice;
      "only a constant is a variable" >:: test_only_a_constant_is_a_variable;
      "a symbol imported twice" >:: test_a_symbol_imported_twice;
      "signed ByteSize and Index" >:: test_signed_byte_size_and_index;
      "universes of arguments" >:: test_universes_of_arguments;
      "the type of a pointer slot" >:: test_the_type_of_a_pointer_slot;
            "a region instance argument" >:: test_a_region_instance_argument;
      "overlap of every pair of kinds" >:: test_overlap_of_every_pair_of_kinds;
      "types across the C boundary" >:: test_types_across_the_c_boundary;
      "a region or type variable is local to no module" >:: test_a_region_or_type_variable_is_local_to_no_module;
      "two matching instances of one module" >:: test_two_matching_instances_of_one_module;
      "entrypoint types of no declaration" >:: test_entrypoint_types_of_no_declaration;
      "a function monomorph without a body" >:: test_a_function_monomorph_without_a_body;
      "a method monomorph without a body" >:: test_a_method_monomorph_without_a_body;
      "a monomorph with bindings for other parameters" >:: test_a_monomorph_with_bindings_for_other_parameters;
      "a method call leaves out an unbound parameter" >:: test_a_method_call_leaves_out_an_unbound_parameter;
      "parameters of region types" >:: test_parameters_of_region_types;
      "borrows of values that are not addresses" >:: test_borrows_of_values_that_are_not_addresses;
      "an imported function of an exported signature" >:: test_an_imported_function_of_an_exported_signature;
      "a struct before other declarations" >:: test_a_struct_before_other_declarations
    ]

let _ = run_test_tt_main suite
