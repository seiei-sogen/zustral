(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Type
open MonoType
open MonoTypeBindings
open Stages.Tast
open Stages.Mtast
open Id
open InstanceIdSet
open LexEnv
open EnvTypes
open EnvUtils
open Error

(** The module environment contains module metadata. *)
type mod_env = mod_rec list

(** The instance method environment contains methods of typeclass instances. *)
type ins_meth_env = ins_meth_rec list

(* IDs and names are plain data. *)
module DeclarationsByName = Map.Make (struct type t = mod_id * identifier let compare: t -> t -> int = Stdlib.compare end)
module RecordsById = Map.Make (struct type t = record_id let compare: t -> t -> int = Stdlib.compare end)
module FunctionsById = Map.Make (struct type t = function_id let compare: t -> t -> int = Stdlib.compare end)

(** The declaration environment contains declarations, the newest first, and
    tables of them, so that the lookups the compiler makes most do not look at
    every declaration (#143). *)
type decl_env = {
    declarations: decl list;
    by_name: decl DeclarationsByName.t;
    records: record_decl RecordsById.t;
    functions: function_decl FunctionsById.t;
  }

(* A declaration with the module and name, or the ID, of an older one takes its
   place in the tables, as it comes before it in the list. *)
let add_declaration (decl: decl) (decls: decl_env): decl_env =
  let (records, functions) =
    match decl with
    | Record record -> (RecordsById.add record.id record decls.records, decls.functions)
    | Function fn -> (decls.records, FunctionsById.add fn.id fn decls.functions)
    | Constant _ | Union _ | UnionCase _ | TypeClass _ | TypeClassMethod _ -> (decls.records, decls.functions)
  in
  {
    declarations = decl :: decls.declarations;
    by_name = DeclarationsByName.add (decl_mod_id decl, decl_name decl) decl decls.by_name;
    records;
    functions;
  }

(** The instance environment contains typeclass instances, which have no name
    and so are not declarations. *)
type instance_env = instance_decl list

(** What a monomorph is a monomorph of. With the type arguments, it is the
    monomorph's key. *)
type monomorph_of =
  | MonomorphOfRecord of record_id
  | MonomorphOfUnion of union_id
  | MonomorphOfFunction of function_id
  | MonomorphOfInstanceMethod of ins_meth_id

module MonomorphKeyMap =
  Map.Make (struct
      type t = monomorph_of * mono_type_bindings
      (* An ID is plain data. *)
      let compare ((of_a, args_a): t) ((of_b, args_b): t): int =
        match Stdlib.compare of_a of_b with
        | 0 -> compare_mono_bindings args_a args_b
        | order -> order
    end)

(* The order of the IDs is the order the monomorphs were registered in: each
   takes the next number of the supply. *)
module MonomorphsById =
  Map.Make (struct
      type t = mono_id
      let compare (a: t) (b: t): int = Int.compare (mono_id_number a) (mono_id_number b)
    end)

(** The monomorph environment contains monomorphs by ID, and the ID of each
    under its key, so that a lookup does not look at every monomorph (#143).
    Each kind of monomorph has its own type of ID, and so its own map of keys. *)
type mono_env = {
    monomorphs: monomorph MonomorphsById.t;
    record_ids: record_mono_id MonomorphKeyMap.t;
    union_ids: union_mono_id MonomorphKeyMap.t;
    function_ids: function_mono_id MonomorphKeyMap.t;
    method_ids: method_mono_id MonomorphKeyMap.t;
  }

type env = Env of {
      mods: mod_env;
      methods: ins_meth_env;
      decls: decl_env;
      instances: instance_env;
      monos: mono_env;
      exports: (function_id * string * ty ForeignType.foreign_signature) list;
      (** The list of exported functions. *)
      supply: Supply.t;
      (** The numbers that have not been given out. Every ID in the
          environment came from here. *)
    }

let empty_env: env =
  let monos: mono_env = {
      monomorphs = MonomorphsById.empty;
      record_ids = MonomorphKeyMap.empty;
      union_ids = MonomorphKeyMap.empty;
      function_ids = MonomorphKeyMap.empty;
      method_ids = MonomorphKeyMap.empty;
    }
  in
  let decls: decl_env = {
      declarations = [];
      by_name = DeclarationsByName.empty;
      records = RecordsById.empty;
      functions = FunctionsById.empty;
    }
  in
  Env { mods = []; methods = []; decls; instances = []; monos; exports = []; supply = Supply.initial; }

(* The lookups by an ID return what they find, not an option. This is the one
   place that reports a miss.

   An ID is made when something is registered in an environment, and what is
   registered stays: `define_monomorph` replaces a monomorph by the same
   monomorph with its definition. The ID of a record or a union is made
   earlier, when it is reserved, and the extraction that reserved it registers
   it or stops at an error. So when a lookup by ID misses, the ID was made by a
   later environment than the one asked, or it was reserved and asked for
   before its record or union was registered. Either is a bug in the
   compiler. *)
let registered (place: place) (what: unit -> string) (found: 'a option): 'a =
  match found with
  | Some thing ->
     thing
  | None ->
     (* As above. The type of an ID does not say which environment made it, or
        whether what it was reserved for is registered yet. *)
     internal_err place ("No " ^ (what ()) ^ " in this environment.")

let get_module_by_id (place: place) (env: env) (mod_id: mod_id): mod_rec =
  let (Env { mods; _ }) = env in
  registered place (fun () -> "module with this ID")
    (List.find_opt (fun (ModRec { id; _ }) -> equal_mod_id mod_id id) mods)

let get_module_by_name (env: env) (mod_name: module_name): mod_rec option =
  let (Env { mods; _ }) = env in
  List.find_opt (fun (ModRec { name; _ }) -> equal_module_name mod_name name) mods

let ensure_no_mod_with_name (env: env) (name: module_name): unit =
  match get_module_by_name env name with
  | Some _ ->
      declaration_error NoSpan (ModuleAlreadyExists name)
  | None ->
     ()

let module_name_from_id (place: place) (env: env) (mod_id: mod_id): module_name =
  let (ModRec { name; _ }) = get_module_by_id place env mod_id in
  name

let add_module (env: env) (input: mod_input): (env * mod_id) =
  let { name; imported_instances; imported_symbols } = input in
  let _ = ensure_no_mod_with_name env name in
  let (Env { mods; methods; decls; instances; monos; exports; supply }) = env in
  let (id, supply) = fresh_mod_id supply in
  let md = ModRec {
               id;
               name;
               imported_instances;
               imported_symbols;
             }
  in
  let env = Env { mods = md :: mods; methods; decls; instances; monos; exports; supply } in
  (env, id)

let get_decl_by_name (place: place) (env: env) (name: sident): decl option =
  match get_module_by_name env (sident_module_name name) with
  | Some (ModRec { id=target_mod_id; _ }) ->
     let (Env { decls; _ }) = env in
     DeclarationsByName.find_opt (target_mod_id, sident_name name) decls.by_name
  | None ->
     declaration_error place (UnknownModule (sident_module_name name))

(* The duplicate rule: a name is taken if it names a built-in type (#51), if the
   module imports it (#28), or if any declaration of the module has it, union
   cases and typeclass methods included (#86). In each case the name would find
   the other thing. This is the only place that has the rule: `register` calls
   it, and so does the extraction of a function, before it checks a foreign
   signature. *)
let name_must_be_free (place: place) (env: env) (mod_id: mod_id) (name: identifier): unit =
  let (ModRec { name = module_name; imported_symbols; _ }) = get_module_by_id place env mod_id in
  match BuiltInNames.type_of_name (ident_string name) with
  | Some _ ->
     declaration_error place (DeclarationNameIsBuiltInType name)
  | None ->
     (match List.find_opt (fun symbol -> equal_identifier (local_name symbol) name) imported_symbols with
      | Some symbol ->
         declaration_error place (DeclarationNameIsImported { name; module_name = source_module_name symbol })
      | None ->
         (match get_decl_by_name place env (make_sident module_name name) with
          | Some _ ->
             declaration_error place (DeclarationAlreadyExists { name; module_name })
          | None ->
             ()))

(* The one way a declaration or an instance method enters the environment. A
   kind of registration that is not checked against the duplicate rule today
   says so in its arm, and a check for it belongs in that arm. *)
let register (type id) (place: place) (env: env) (registration: id registration): (env * id) =
  let (Env { mods; methods; decls; instances; monos; exports; supply }) = env in
  let name_must_be_free (mod_id: mod_id) (name: identifier): unit =
    name_must_be_free place env mod_id name
  in
  let add_decl (type kind_id) (fresh: Supply.t -> kind_id * Supply.t) (make: kind_id -> decl): (env * kind_id) =
    let (id, supply) = fresh supply in
    (Env { mods; methods; decls = add_declaration (make id) decls; instances; monos; exports; supply }, id)
  in
  (* A record or a union has its ID already. It still takes the number a
     declaration takes, and leaves it unused, so that the declarations after it
     have the numbers they had before its ID was reserved: a function's number
     is in the name the generated C gives it. *)
  let add_reserved (type kind_id) (has_id: decl -> bool) (id: kind_id) (make: kind_id -> decl): (env * kind_id) =
    if List.exists has_id decls.declarations then
      (* A reserved ID stands for one declaration. Nothing in its type says
         that it was not used yet. *)
      internal_err place "A record or a union is already registered under this reserved ID."
    else
      add_decl (fun supply -> (id, snd (Supply.next_decl_id supply))) make
  in
  match registration with
  | NewConstant { mod_id; vis; name; ty; docstring } ->
     name_must_be_free mod_id name;
     add_decl fresh_constant_id (fun id -> Constant { id; mod_id; vis; name; ty; docstring })
  | NewRecord { id; mod_id; vis; name; docstring; typarams; universe; slots } ->
     name_must_be_free mod_id name;
     let has_id (decl: decl): bool =
       match decl with
       | Record record -> equal_record_id record.id id
       | Constant _ | Union _ | UnionCase _ | Function _ | TypeClass _ | TypeClassMethod _ -> false
     in
     add_reserved has_id id (fun id -> Record { id; mod_id; vis; name; docstring; typarams; universe; slots })
  | NewUnion { id; mod_id; vis; name; docstring; typarams; universe } ->
     name_must_be_free mod_id name;
     let has_id (decl: decl): bool =
       match decl with
       | Union union -> equal_union_id union.id id
       | Constant _ | Record _ | UnionCase _ | Function _ | TypeClass _ | TypeClassMethod _ -> false
     in
     add_reserved has_id id (fun id -> Union { id; mod_id; vis; name; docstring; typarams; universe })
  | NewUnionCase { mod_id; vis; union_id; name; docstring; slots } ->
     name_must_be_free mod_id name;
     add_decl fresh_union_case_id (fun id -> UnionCase { id; vis; mod_id; union_id; name; docstring; slots })
  | NewFunction { mod_id; span; vis; name; docstring; typarams; value_params; rt; impl } ->
     name_must_be_free mod_id name;
     add_decl fresh_function_id (fun id -> Function { id; mod_id; span; vis; name; docstring; typarams; value_params; rt; impl })
  | NewTypeClass { mod_id; vis; name; docstring; param } ->
     name_must_be_free mod_id name;
     add_decl fresh_typeclass_id (fun id -> TypeClass { id; mod_id; vis; name; docstring; param })
  | NewTypeClassMethod { mod_id; vis; typeclass_id; name; docstring; typarams; value_params; rt } ->
     name_must_be_free mod_id name;
     add_decl fresh_typeclass_method_id (fun id -> TypeClassMethod { id; mod_id; vis; typeclass_id; name; docstring; typarams; value_params; rt })
  | NewInstance { mod_id; vis; typeclass_id; docstring; typarams; argument } ->
     (* Not checked here: an instance has no name. The caller checks that it
        overlaps no instance of its own module, and the orphan rule, which
        keeps instances of two modules from overlapping. *)
     let (id, supply) = fresh_instance_id supply in
     let instance: instance_decl = { id; mod_id; vis; typeclass_id; docstring; typarams; argument } in
     (Env { mods; methods; decls; instances = instance :: instances; monos; exports; supply }, id)
  | NewInstanceMethod { instance_id; method_id; docstring; name; typarams; value_params; rt; body } ->
     (* Not checked here. The caller checks the methods of the instance against
        the methods of its typeclass (#91). *)
     let (id, supply) = fresh_ins_meth_id supply in
     let meth = InsMethRec { id; instance_id; method_id; docstring; name; typarams; value_params; rt; body } in
     (Env { mods; methods = meth :: methods; decls; instances; monos; exports; supply }, id)

let reserve_record_id (env: env): (env * record_id) =
  let (Env { mods; methods; decls; instances; monos; exports; supply }) = env in
  let (id, supply) = fresh_record_id supply in
  (Env { mods; methods; decls; instances; monos; exports; supply }, id)

let reserve_union_id (env: env): (env * union_id) =
  let (Env { mods; methods; decls; instances; monos; exports; supply }) = env in
  let (id, supply) = fresh_union_id supply in
  (Env { mods; methods; decls; instances; monos; exports; supply }, id)

(* The lookups by an ID that says its kind. Each looks only at the declarations
   of that kind, so what it returns is one, and it misses in the one case that
   `registered` reports. *)

let function_of (decl: decl): function_decl option =
  match decl with
  | Function fn -> Some fn
  | Constant _ | Record _ | Union _ | UnionCase _ | TypeClass _ | TypeClassMethod _ -> None

let get_function (env: env) (id: function_id): function_decl =
  let (Env { decls; _ }) = env in
  registered NoSpan (fun () -> "function with ID `" ^ (show_function_id id) ^ "`")
    (FunctionsById.find_opt id decls.functions)

let typeclass_of (decl: decl): typeclass_decl option =
  match decl with
  | TypeClass typeclass -> Some typeclass
  | Constant _ | Record _ | Union _ | UnionCase _ | Function _ | TypeClassMethod _ -> None

let get_typeclass (place: place) (env: env) (id: typeclass_id): typeclass_decl =
  let (Env { decls; _ }) = env in
  let has_id (typeclass: typeclass_decl): bool = equal_typeclass_id typeclass.id id in
  registered place (fun () -> "typeclass with ID `" ^ (show_typeclass_id id) ^ "`")
    (List.find_opt has_id (List.filter_map typeclass_of decls.declarations))

let get_record (env: env) (id: record_id): record_decl =
  let (Env { decls; _ }) = env in
  registered NoSpan (fun () -> "record with ID `" ^ (show_record_id id) ^ "`")
    (RecordsById.find_opt id decls.records)

let union_of (decl: decl): union_decl option =
  match decl with
  | Union union -> Some union
  | Constant _ | Record _ | UnionCase _ | Function _ | TypeClass _ | TypeClassMethod _ -> None

let get_union (place: place) (env: env) (id: union_id): union_decl =
  let (Env { decls; _ }) = env in
  let has_id (union: union_decl): bool = equal_union_id union.id id in
  registered place (fun () -> "union with ID `" ^ (show_union_id id) ^ "`")
    (List.find_opt has_id (List.filter_map union_of decls.declarations))

let union_case_of (decl: decl): union_case_decl option =
  match decl with
  | UnionCase case -> Some case
  | Constant _ | Record _ | Union _ | Function _ | TypeClass _ | TypeClassMethod _ -> None

let get_instance_decl (place: place) (env: env) (id: instance_id): instance_decl =
  let (Env { instances; _ }) = env in
  let has_id (instance: instance_decl): bool = equal_instance_id instance.id id in
  registered place (fun () -> "instance with ID `" ^ (show_instance_id id) ^ "`")
    (List.find_opt has_id instances)

let get_instance_method (env: env) (ins_meth_id: ins_meth_id): ins_meth_rec =
  let (Env { methods; _ }) = env in
  registered NoSpan (fun () -> "instance method with ID `" ^ (show_ins_meth_id ins_meth_id) ^ "`")
    (List.find_opt (fun (InsMethRec { id; _ }) -> equal_ins_meth_id id ins_meth_id) methods)

let get_typeclass_methods (env: env) (typeclass_id: typeclass_id): typeclass_method_decl list =
  let (Env { decls; _ }) = env in
  let pick (decl: decl): typeclass_method_decl option =
    match decl with
    | TypeClassMethod meth ->
       if equal_typeclass_id meth.typeclass_id typeclass_id then
         Some meth
       else
         None
    | Constant _ | Record _ | Union _ | UnionCase _ | Function _ | TypeClass _ ->
       None
  in
  (* The newest declaration is first. *)
  List.rev (List.filter_map pick decls.declarations)

let module_instances (env: env) (id: mod_id): instance_decl list =
  let (Env { instances; _ }) = env in
  List.filter (fun (instance: instance_decl) -> equal_mod_id instance.mod_id id) instances

let module_public_instances (env: env) (id: mod_id): instance_decl list =
  let is_public (instance: instance_decl): bool =
    match instance.vis with
    | VisPublic -> true
    | VisPrivate -> false
  in
  List.filter is_public (module_instances env id)

let get_union_cases (env: env) (id: union_id): union_case_decl list =
  let (Env { decls; _ }) = env in
  List.filter (fun (case: union_case_decl) -> equal_union_id case.union_id id) (List.filter_map union_case_of decls.declarations)

let get_callable (place: place) (env: env) (importing_module_name: module_name) (name: sident): callable option =
  let _ = importing_module_name in
  match get_decl_by_name place env name with
  | Some decl ->
     (match decl with
      | Record { id; typarams; universe; slots; _ } ->
         Some (RecordConstructor (id, typarams, universe, slots))
      | UnionCase { name; union_id; slots; _ } ->
         let { typarams; universe; _ }: union_decl = get_union place env union_id in
         Some (
             UnionConstructor {
                 union_id = union_id;
                 type_params = typarams;
                 universe = universe;
                 case = TypedCase (name, slots)
           })
      | Function { id; typarams; value_params; rt; _ } ->
         Some (FunctionCallable (id, typarams, value_params, rt))
      | TypeClassMethod { typeclass_id; value_params; rt; _ } ->
         Some (
             MethodCallable {
                 typeclass_id = typeclass_id;
                 value_parameters = value_params;
                 return_type = rt;
               }
           )
      | Constant _ | Union _ | TypeClass _ ->
         None)
  | None ->
     None

let get_variable (place: place) (env: env) (lexenv: lexenv) (name: qident): (ty * variable_source) option =
  (* A variable has the name the program wrote, which is the nickname of an
     import of that name. *)
  match get_var lexenv (local_name name) with
  | Some (ty, src) ->
     Some (ty, VarLexical src)
  | None ->
     (match get_decl_by_name place env (qident_to_sident name) with
      | Some decl ->
         (match decl with
          | Constant { ty; _ } ->
             Some (ty, VarConstant)
          | Record _ | Union _ | UnionCase _ | Function _ | TypeClass _
          | TypeClassMethod _ ->
             None)
      | None ->
         None)

let visible_instances (place: place) (env: env) (id: mod_id): instance_decl list =
  let instances_imported_by_module: instance_decl list =
    let (ModRec { imported_instances; _ }) = get_module_by_id place env id in
    List.map (get_instance_decl place env) (List.of_seq (InstanceIdSet.to_seq imported_instances))
  in
  (module_instances env id) @ instances_imported_by_module

let all_instances (env: env): instance_decl list =
  let (Env { instances; _ }) = env in
  instances

let store_function_body (env: env) (fn_id: function_id) (body: tstmt): env =
  let (Env { mods; methods; decls; instances; monos; exports; supply }) = env in
  let fn: function_decl = get_function env fn_id in
  let new_decl: decl =
    match fn.impl with
    (* A foreign function is typed as TForeignFunction, which has no body to
       store. *)
    | Body _ | Foreign _ | ForeignGeneric ->
       (* Depends on the stage: the body extraction pass stores each body once.
          The type of a declaration does not say whether that pass has run. *)
       internal_err NoSpan ("function `" ^ (ident_string fn.name) ^ "` already has a body")
    | BodyNotAttached ->
       Function { fn with impl = Body body }
  in
  let is_existing_one (d: decl): bool =
    match function_of d with
    | Some existing -> equal_function_id existing.id fn_id
    | None -> false
  in
  let decls_without_existing_one: decl list = List.filter (fun (d: decl) -> not (is_existing_one d)) decls.declarations in
  let decls = add_declaration new_decl { decls with declarations = decls_without_existing_one } in
  let env = Env { mods; methods; decls; instances; monos; exports; supply } in
  env

let rec store_method_body (env: env) (ins_meth_id: ins_meth_id) (body: tstmt): env =
  let (Env { mods; methods; decls; instances; monos; exports; supply }) = env in
  let meth: ins_meth_rec = get_instance_method env ins_meth_id in
  let new_method: ins_meth_rec = replace_method_body meth body in
  let methods_without_existing_one: ins_meth_rec list = List.filter (fun (InsMethRec { id; _ }) -> not (equal_ins_meth_id id ins_meth_id)) methods in
  let env = Env { mods; methods = new_method :: methods_without_existing_one; decls; instances; monos; exports; supply } in
  env

and replace_method_body (meth: ins_meth_rec) (new_body: tstmt): ins_meth_rec =
  let (InsMethRec { id; instance_id; method_id; docstring; name; typarams; value_params; rt; body }) = meth in
  (match body with
   | Some _ ->
      (* As in store_function_body: depends on the stage. *)
      internal_err NoSpan ("function `" ^ (ident_string name) ^ "` already has a body")
   | None ->
      InsMethRec { id; instance_id; method_id; docstring; name; typarams; value_params; rt; body=(Some new_body) })

let get_instance_method_from_instance_id_and_method_name (place: place) (env: env) (instance_id: instance_id) (name: identifier): ins_meth_rec =
  let (Env { methods; _ }) = env in
  let pred (InsMethRec { instance_id=target_instance_id; name=target_name; _ }): bool =
    (equal_instance_id instance_id target_instance_id)
    && (equal_identifier name target_name)
  in
  (* The declaration of an instance defines every method of its typeclass (#91). *)
  registered place (fun () -> "method `" ^ (ident_string name) ^ "` of the instance with ID `" ^ (show_instance_id instance_id) ^ "`")
    (List.find_opt pred methods)

let add_record_monomorph (env: env) (type_id: record_id) (tyargs: mono_type_bindings): (env * record_mono_id) =
  let (Env { mods; methods; decls; instances; monos; exports; supply }) = env in
  let (id, supply) = fresh_record_mono_id supply in
  let mono = MonoRecordDefinition { id; type_id; tyargs; slots = None } in
  let monos = { monos with monomorphs = MonomorphsById.add (mono_id_of_record id) mono monos.monomorphs; record_ids = MonomorphKeyMap.add (MonomorphOfRecord type_id, tyargs) id monos.record_ids } in
  let env = Env { mods; methods; decls; instances; monos; exports; supply } in
  (env, id)

let add_union_monomorph (env: env) (type_id: union_id) (tyargs: mono_type_bindings): (env * union_mono_id) =
  let (Env { mods; methods; decls; instances; monos; exports; supply }) = env in
  let (id, supply) = fresh_union_mono_id supply in
  let mono = MonoUnionDefinition { id; type_id; tyargs; cases = None } in
  let monos = { monos with monomorphs = MonomorphsById.add (mono_id_of_union id) mono monos.monomorphs; union_ids = MonomorphKeyMap.add (MonomorphOfUnion type_id, tyargs) id monos.union_ids } in
  let env = Env { mods; methods; decls; instances; monos; exports; supply } in
  (env, id)

let add_function_monomorph (env: env) (function_id: function_id) (tyargs: mono_type_bindings): (env * function_mono_id) =
  let (Env { mods; methods; decls; instances; monos; exports; supply }) = env in
  let (id, supply) = fresh_function_mono_id supply in
  let mono = MonoFunction { id; function_id; tyargs; body = None } in
  let monos = { monos with monomorphs = MonomorphsById.add (mono_id_of_function id) mono monos.monomorphs; function_ids = MonomorphKeyMap.add (MonomorphOfFunction function_id, tyargs) id monos.function_ids } in
  let env = Env { mods; methods; decls; instances; monos; exports; supply } in
  (env, id)

let add_instance_method_monomorph (env: env) (method_id: ins_meth_id) (tyargs: mono_type_bindings): (env * method_mono_id) =
  let (Env { mods; methods; decls; instances; monos; exports; supply }) = env in
  let (id, supply) = fresh_method_mono_id supply in
  let mono = MonoInstanceMethod { id; method_id; tyargs; body = None } in
  let monos = { monos with monomorphs = MonomorphsById.add (mono_id_of_method id) mono monos.monomorphs; method_ids = MonomorphKeyMap.add (MonomorphOfInstanceMethod method_id, tyargs) id monos.method_ids } in
  let env = Env { mods; methods; decls; instances; monos; exports; supply } in
  (env, id)

let get_monomorph (env: env) (id: mono_id): monomorph =
  let (Env { monos; _ }) = env in
  registered NoSpan (fun () -> "monomorph with ID `" ^ (show_mono_id id) ^ "`")
    (MonomorphsById.find_opt id monos.monomorphs)

let get_type_monomorph (env: env) (id: mono_id): (mod_id * TypeSignature.type_signature * mono_type_bindings) =
  let (Env { monos; _ }) = env in
  let pick (mono: monomorph): (mod_id * TypeSignature.type_signature * mono_type_bindings) option =
    match mono with
    | MonoRecordDefinition { type_id; tyargs; _ } ->
       let { mod_id; name; typarams; universe; _ }: record_decl = get_record env type_id in
       Some (mod_id, TypeSignature.TypeSignature (name, typarams, universe, RecordTypeId type_id), tyargs)
    | MonoUnionDefinition { type_id; tyargs; _ } ->
       let { mod_id; name; typarams; universe; _ }: union_decl = get_union NoSpan env type_id in
       Some (mod_id, TypeSignature.TypeSignature (name, typarams, universe, UnionTypeId type_id), tyargs)
    | MonoFunction _ | MonoInstanceMethod _ ->
       None
  in
  (* `MonoNamedType` holds an ID of any kind of monomorph, so the ID of a
     function's monomorph also misses here. *)
  registered NoSpan (fun () -> "record or union monomorph with ID `" ^ (show_mono_id id) ^ "`")
    (Option.bind (MonomorphsById.find_opt id monos.monomorphs) pick)

(* A monomorph is also found by its key: what it is a monomorph of, and the
   type arguments. A key is not an ID. It can name a monomorph that no pass has
   registered yet, so these lookups return an option. `add_or_get_*` register
   the monomorph the first time its key is asked for. *)
let get_record_monomorph (env: env) (record_id: record_id) (args: mono_type_bindings): record_mono_id option =
  let (Env { monos; _ }) = env in
  MonomorphKeyMap.find_opt (MonomorphOfRecord record_id, args) monos.record_ids

let get_union_monomorph (env: env) (union_id: union_id) (args: mono_type_bindings): union_mono_id option =
  let (Env { monos; _ }) = env in
  MonomorphKeyMap.find_opt (MonomorphOfUnion union_id, args) monos.union_ids

let add_or_get_record_monomorph (env: env) (record_id: record_id) (args: mono_type_bindings): (env * record_mono_id) =
  match get_record_monomorph env record_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_record_monomorph env record_id args

let add_or_get_union_monomorph (env: env) (union_id: union_id) (args: mono_type_bindings): (env * union_mono_id) =
  match get_union_monomorph env union_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_union_monomorph env union_id args

let find_function_monomorph (env: env) (decl_id: function_id) (args: mono_type_bindings): function_mono_id option =
  let (Env { monos; _ }) = env in
  MonomorphKeyMap.find_opt (MonomorphOfFunction decl_id, args) monos.function_ids

let add_or_get_function_monomorph (env: env) (decl_id: function_id) (args: mono_type_bindings): (env * function_mono_id) =
  match find_function_monomorph env decl_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_function_monomorph env decl_id args

let find_instance_method_monomorph (env: env) (method_id: ins_meth_id) (args: mono_type_bindings): method_mono_id option =
  let (Env { monos; _ }) = env in
  MonomorphKeyMap.find_opt (MonomorphOfInstanceMethod method_id, args) monos.method_ids

let add_or_get_instance_method_monomorph (env: env) (method_id: ins_meth_id) (args: mono_type_bindings): (env * method_mono_id) =
  match find_instance_method_monomorph env method_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_instance_method_monomorph env method_id args

let is_instantiated (mono: monomorph): bool =
  match mono with
  | MonoRecordDefinition { slots; _ } ->
    Option.is_some slots
  | MonoUnionDefinition { cases; _ } ->
    Option.is_some cases
  | MonoFunction { body; _ } ->
    Option.is_some body
  | MonoInstanceMethod { body; _ } ->
    Option.is_some body

(* In the order they were registered. This order decides the order in which
   later monomorphs are registered, and so their IDs, which are in the names
   the generated C gives them. *)
let get_uninstantiated_monomorphs (env: env): monomorph list =
  let (Env { monos; _ }) = env in
  let pred (m: monomorph) =
    not (is_instantiated m)
  in
  List.filter pred (List.map snd (MonomorphsById.bindings monos.monomorphs))

(* Give a monomorph its definition. `define` returns the defined monomorph, or
   None when the monomorph with the ID is of another kind than the definition:
   only an ID of another lineage of environments finds one. *)
let define_monomorph (env: env) (id: mono_id) (define: monomorph -> monomorph option): env =
  let (Env { mods; methods; decls; instances; monos; exports; supply }) = env in
  let defined: monomorph =
    registered NoSpan (fun () -> "monomorph with ID `" ^ (show_mono_id id) ^ "`")
      (Option.bind (MonomorphsById.find_opt id monos.monomorphs) define)
  in
  Env { mods; methods; decls; instances; monos = { monos with monomorphs = MonomorphsById.add id defined monos.monomorphs }; exports; supply }

let store_record_monomorph_definition (env: env) (id: record_mono_id) (new_slots: mono_slot list): env =
  define_monomorph env (mono_id_of_record id) (fun (mono: monomorph) ->
      match mono with
      | MonoRecordDefinition { type_id; tyargs; _ } ->
        Some (MonoRecordDefinition { id; type_id; tyargs; slots = Some new_slots; })
      | MonoUnionDefinition _ | MonoFunction _ | MonoInstanceMethod _ ->
        None)

let store_union_monomorph_definition (env: env) (id: union_mono_id) (new_cases: mono_case list): env =
  define_monomorph env (mono_id_of_union id) (fun (mono: monomorph) ->
      match mono with
      | MonoUnionDefinition { type_id; tyargs; _ } ->
        Some (MonoUnionDefinition { id; type_id; tyargs; cases = Some new_cases; })
      | MonoRecordDefinition _ | MonoFunction _ | MonoInstanceMethod _ ->
        None)

let store_function_monomorph_definition (env: env) (id: function_mono_id) (new_body: mstmt): env =
  define_monomorph env (mono_id_of_function id) (fun (mono: monomorph) ->
      match mono with
      | MonoFunction { function_id; tyargs; _ } ->
        Some (MonoFunction { id; function_id; tyargs; body = Some new_body; })
      | MonoRecordDefinition _ | MonoUnionDefinition _ | MonoInstanceMethod _ ->
        None)

let store_instance_method_monomorph_definition (env: env) (id: method_mono_id) (new_body: mstmt): env =
  define_monomorph env (mono_id_of_method id) (fun (mono: monomorph) ->
      match mono with
      | MonoInstanceMethod { method_id; tyargs; _ } ->
        Some (MonoInstanceMethod { id; method_id; tyargs; body = Some new_body; })
      | MonoRecordDefinition _ | MonoUnionDefinition _ | MonoFunction _ ->
        None)

let add_exported_function (env: env) (id: function_id) (export_name: string) (signature: ty ForeignType.foreign_signature): env =
  let (Env { mods; methods; decls; instances; monos; exports; supply }) = env in
  let env = Env { mods; methods; decls; instances; monos; exports = (id, export_name, signature) :: exports; supply } in
  env

let get_export_functions (env: env): (function_id * string * ty ForeignType.foreign_signature) list =
  let (Env { exports; _ }) = env in
  exports

let with_supply (env: env) (stage: Supply.t -> ('a * Supply.t) Outcome.t): (env * 'a) Outcome.t =
  let (Env { mods; methods; decls; instances; monos; exports; supply }) = env in
  Outcome.map
    (fun (result, supply) -> (Env { mods; methods; decls; instances; monos; exports; supply }, result))
    (stage supply)
