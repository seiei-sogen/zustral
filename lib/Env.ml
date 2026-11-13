(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
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
open DeclIdSet
open LexEnv
open EnvTypes
open EnvUtils
open Error

(** The module environment contains module metadata. *)
type mod_env = mod_rec list

(** The instance method environment contains methods of typeclass instances. *)
type ins_meth_env = ins_meth_rec list

(** The declaration environment contains declarations. *)
type decl_env = decl list

(** The monomorph environment contains monomorphs. *)
type mono_env = monomorph list

type env = Env of {
      mods: mod_env;
      methods: ins_meth_env;
      decls: decl_env;
      monos: mono_env;
      exports: (decl_id * string) list;
      (** The list of exported functions. *)
      supply: Supply.t;
      (** The numbers that have not been given out. Every ID in the
          environment came from here. *)
    }

let empty_env: env =
  Env { mods = []; methods = []; decls = []; monos = []; exports = []; supply = Supply.initial; }

(* The lookups whose key the compiler made itself return what they find, not an
   option. This is the one place that reports a miss.

   An ID is made only when something is registered in an environment, and
   nothing is ever taken out of one. So when a lookup by ID misses, the ID was
   made by a later environment than the one asked, which is a bug in the
   compiler. *)
let registered (what: unit -> string) (found: 'a option): 'a =
  match found with
  | Some thing ->
     thing
  | None ->
     internal_err ("No " ^ (what ()) ^ " in this environment.")

let get_module_by_id (env: env) (mod_id: mod_id): mod_rec =
  let (Env { mods; _ }) = env in
  registered (fun () -> "module with this ID")
    (List.find_opt (fun (ModRec { id; _ }) -> equal_mod_id mod_id id) mods)

let get_module_by_name (env: env) (mod_name: module_name): mod_rec option =
  let (Env { mods; _ }) = env in
  List.find_opt (fun (ModRec { name; _ }) -> equal_module_name mod_name name) mods

let ensure_no_mod_with_name (env: env) (name: module_name): unit =
  match get_module_by_name env name with
  | Some _ ->
      declaration_error (ModuleAlreadyExists name)
  | None ->
     ()

let module_name_from_id (env: env) (mod_id: mod_id): module_name =
  let (ModRec { name; _ }) = get_module_by_id env mod_id in
  name

let add_module (env: env) (input: mod_input): (env * mod_id) =
  let { name; imported_instances; imports_from } = input in
  let _ = ensure_no_mod_with_name env name in
  let (Env { mods; methods; decls; monos; exports; supply }) = env in
  let (id, supply) = fresh_mod_id supply in
  let md = ModRec {
               id;
               name;
               imported_instances;
               imports_from;
             }
  in
  let env = Env { mods = md :: mods; methods; decls; monos; exports; supply } in
  (env, id)

let get_decl_by_name (env: env) (name: sident): decl option =
  match get_module_by_name env (sident_module_name name) with
  | Some (ModRec { id=target_mod_id; _ }) ->
     let (Env { decls; _ }) = env in
     let pred (d: decl) =
       let mid = decl_mod_id d in
       if equal_mod_id target_mod_id mid then
         (* Module matches. *)
         (match (decl_name d) with
          | (Some dname) ->
             (* Does the name match? *)
             equal_identifier dname (sident_name name)
          | None ->
             false)
       else
         false
     in
     List.find_opt (fun d -> pred d) decls
  | None ->
     declaration_error (UnknownModule (sident_module_name name))

(* The one way a declaration or an instance method enters the environment. The
   rule about names that are already taken is here and nowhere else. A kind of
   registration that is not checked today says so in its arm, and a check for
   it belongs in that arm. *)
let register (type id) (env: env) (registration: id registration): (env * id) =
  let (Env { mods; methods; decls; monos; exports; supply }) = env in
  (* The duplicate rule: a name is taken if any declaration of the module has
     it, union cases and typeclass methods included. The rule does not look at
     the names the module imports (#28) or at the names of the built-in types
     (#51). *)
  let name_must_be_free (mod_id: mod_id) (name: identifier): unit =
    let module_name: module_name = module_name_from_id env mod_id in
    match get_decl_by_name env (make_sident module_name name) with
    | Some _ ->
       declaration_error (DeclarationAlreadyExists { name; module_name })
    | None ->
       ()
  in
  let add_decl (make: decl_id -> decl): (env * decl_id) =
    let (id, supply) = fresh_decl_id supply in
    (Env { mods; methods; decls = (make id) :: decls; monos; exports; supply }, id)
  in
  match registration with
  | NewConstant { mod_id; vis; name; ty; docstring } ->
     name_must_be_free mod_id name;
     add_decl (fun id -> Constant { id; mod_id; vis; name; ty; docstring })
  | NewRecord { mod_id; vis; name; docstring; typarams; universe; slots } ->
     name_must_be_free mod_id name;
     add_decl (fun id -> Record { id; mod_id; vis; name; docstring; typarams; universe; slots })
  | NewUnion { mod_id; vis; name; docstring; typarams; universe } ->
     name_must_be_free mod_id name;
     add_decl (fun id -> Union { id; mod_id; vis; name; docstring; typarams; universe })
  | NewUnionCase { mod_id; vis; union_id; name; docstring; slots } ->
     (* Not checked (#86): a case can take the name of another case, or of any
        other declaration of the module. *)
     add_decl (fun id -> UnionCase { id; vis; mod_id; union_id; name; docstring; slots })
  | NewFunction { mod_id; vis; name; docstring; typarams; value_params; rt; external_name; body } ->
     name_must_be_free mod_id name;
     add_decl (fun id -> Function { id; mod_id; vis; name; docstring; typarams; value_params; rt; external_name; body })
  | NewTypeClass { mod_id; vis; name; docstring; param } ->
     name_must_be_free mod_id name;
     add_decl (fun id -> TypeClass { id; mod_id; vis; name; docstring; param })
  | NewTypeClassMethod { mod_id; vis; typeclass_id; name; docstring; typarams; value_params; rt } ->
     (* Not checked (#86): a method can take the name of a method of another
        typeclass, or of any other declaration of the module. *)
     add_decl (fun id -> TypeClassMethod { id; mod_id; vis; typeclass_id; name; docstring; typarams; value_params; rt })
  | NewInstance { mod_id; vis; typeclass_id; docstring; typarams; argument } ->
     (* Not checked here: an instance has no name. The caller checks that it
        overlaps no instance of its own module, and the orphan rule. The orphan
        rule takes a built-in type to be local to every module, so instances
        from two modules can overlap (#72). *)
     add_decl (fun id -> Instance { id; mod_id; vis; typeclass_id; docstring; typarams; argument })
  | NewInstanceMethod { instance_id; method_id; docstring; name; typarams; value_params; rt; body } ->
     (* Not checked here. The caller checks that the typeclass has a method of
        this name. Nothing checks that an instance defines every method of its
        typeclass (#91), or that it defines a method once. *)
     let (id, supply) = fresh_ins_meth_id supply in
     let meth = InsMethRec { id; instance_id; method_id; docstring; name; typarams; value_params; rt; body } in
     (Env { mods; methods = meth :: methods; decls; monos; exports; supply }, id)

let get_decl_by_id (env: env) (id: decl_id): decl =
  let (Env { decls; _ }) = env in
  registered (fun () -> "declaration with ID `" ^ (show_decl_id id) ^ "`")
    (List.find_opt (fun d -> equal_decl_id id (decl_id d)) decls)

let get_instance_method (env: env) (ins_meth_id: ins_meth_id): ins_meth_rec =
  let (Env { methods; _ }) = env in
  registered (fun () -> "instance method with ID `" ^ (show_ins_meth_id ins_meth_id) ^ "`")
    (List.find_opt (fun (InsMethRec { id; _ }) -> equal_ins_meth_id id ins_meth_id) methods)

let get_method_from_typeclass_id_and_name (env: env) (typeclass_id: decl_id) (name: identifier): decl option =
  let (Env { decls; _ }) = env in
  let pred (decl: decl): bool =
    match decl with
    | TypeClassMethod { typeclass_id=target_typeclass_id; name=target_name; _ } ->
       (equal_decl_id target_typeclass_id typeclass_id)
       && (equal_identifier target_name name)
    | _ ->
       false
  in
  List.find_opt pred decls

let module_instances (env: env) (id: mod_id): decl list =
  let (Env { decls; _ }) = env
  and pred = function
    | Instance { mod_id; _ } ->
       equal_mod_id mod_id id
    | _ -> false
  in
  List.filter pred decls

let module_public_instances (env: env) (id: mod_id): decl list =
  let (Env { decls; _ }) = env
  and pred = function
    | Instance { mod_id; vis; _ } ->
       (match vis with
        | VisPublic -> equal_mod_id mod_id id
        | VisPrivate -> false)
    | _ -> false
  in
  List.filter pred decls

let get_union_cases (env: env) (id: decl_id): decl list =
  let (Env { decls; _ }) = env
  and pred = function
    | UnionCase { union_id; _ } ->
       equal_decl_id union_id id
    | _ -> false
  in
  List.filter pred decls

let get_callable (env: env) (importing_module_name: module_name) (name: sident): callable option =
  let _ = importing_module_name in
  match get_decl_by_name env name with
  | Some decl ->
     (match decl with
      | Record { id; typarams; universe; slots; _ } ->
         Some (RecordConstructor (id, typarams, universe, slots))
      | UnionCase { name; union_id; slots; _ } ->
         let (typarams, universe) =
           (match get_decl_by_id env union_id with
            | Union { typarams; universe; _} -> (typarams, universe)
            | _ -> generic_error NoUnionWithId)
         in
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
      | _ ->
         None)
  | None ->
     None

let get_variable (env: env) (lexenv: lexenv) (name: qident): (ty * var_source) option =
  match get_var lexenv (original_name name) with
  | Some (ty, src) ->
     Some (ty, src)
  | None ->
     (match get_decl_by_name env (qident_to_sident name) with
      | Some decl ->
         (match decl with
          | Constant { ty; _ } ->
             Some (ty, VarConstant)
          | _ ->
             None)
      | None ->
         None)

let visible_instances (env: env) (id: mod_id): decl list =
  let instances_defined_in_module: decl list =
    let (Env { decls; _ }) = env
    and pred = function
      | Instance { mod_id; _ } ->
         equal_mod_id id mod_id
      | _ -> false
    in
    List.filter pred decls
  and instances_imported_by_module: decl list =
    let (ModRec { imported_instances; _ }) = get_module_by_id env id in
    List.map (fun id -> get_decl_by_id env id) (List.of_seq (DeclIdSet.to_seq imported_instances))
  in
  instances_defined_in_module @ instances_imported_by_module

let rec store_function_body (env: env) (fn_id: decl_id) (body: tstmt): env =
  let (Env { mods; methods; decls; monos; exports; supply }) = env in
  let decl: decl = get_decl_by_id env fn_id in
  let new_decl: decl = replace_function_body decl body in
  let decls_without_existing_one: decl list = List.filter (fun (d: decl) -> not (equal_decl_id (decl_id d) fn_id)) decls in
  let env = Env { mods; methods; decls = new_decl :: decls_without_existing_one; monos; exports; supply } in
  env

and replace_function_body (decl: decl) (new_body: tstmt): decl =
  match decl with
  | Function { id; mod_id; vis; name; docstring; typarams; value_params; rt; external_name; body } ->
     (match body with
      | Some _ ->
         internal_err ("function `" ^ (ident_string name) ^ "` already has a body")
      | None ->
         Function { id; mod_id; vis; name; docstring; typarams; value_params; rt; external_name; body=(Some new_body) })
  | _ ->
     internal_err "not a function."

let rec store_method_body (env: env) (ins_meth_id: ins_meth_id) (body: tstmt): env =
  let (Env { mods; methods; decls; monos; exports; supply }) = env in
  let meth: ins_meth_rec = get_instance_method env ins_meth_id in
  let new_method: ins_meth_rec = replace_method_body meth body in
  let methods_without_existing_one: ins_meth_rec list = List.filter (fun (InsMethRec { id; _ }) -> not (equal_ins_meth_id id ins_meth_id)) methods in
  let env = Env { mods; methods = new_method :: methods_without_existing_one; decls; monos; exports; supply } in
  env

and replace_method_body (meth: ins_meth_rec) (new_body: tstmt): ins_meth_rec =
  let (InsMethRec { id; instance_id; method_id; docstring; name; typarams; value_params; rt; body }) = meth in
  (match body with
   | Some _ ->
      internal_err ("function `" ^ (ident_string name) ^ "` already has a body")
   | None ->
      InsMethRec { id; instance_id; method_id; docstring; name; typarams; value_params; rt; body=(Some new_body) })

let get_instance_method_from_instance_id_and_method_name (env: env) (instance_id: decl_id) (name: identifier): ins_meth_rec option =
  let (Env { methods; _ }) = env in
  let pred (InsMethRec { instance_id=target_instance_id; name=target_name; _ }): bool =
    (equal_decl_id instance_id target_instance_id)
    && (equal_identifier name target_name)
  in
  List.find_opt pred methods

let add_record_monomorph (env: env) (type_id: decl_id) (tyargs: mono_type_bindings): (env * mono_id) =
  let (Env { mods; methods; decls; monos; exports; supply }) = env in
  let (id, supply) = fresh_mono_id supply in
  let mono = MonoRecordDefinition { id; type_id; tyargs; slots = None } in
  let env = Env { mods; methods; decls; monos = mono :: monos; exports; supply } in
  (env, id)

let add_union_monomorph (env: env) (type_id: decl_id) (tyargs: mono_type_bindings): (env * mono_id) =
  let (Env { mods; methods; decls; monos; exports; supply }) = env in
  let (id, supply) = fresh_mono_id supply in
  let mono = MonoUnionDefinition { id; type_id; tyargs; cases = None } in
  let env = Env { mods; methods; decls; monos = mono :: monos; exports; supply } in
  (env, id)

let add_function_monomorph (env: env) (function_id: decl_id) (tyargs: mono_type_bindings): (env * mono_id) =
  let (Env { mods; methods; decls; monos; exports; supply }) = env in
  let (id, supply) = fresh_mono_id supply in
  let mono = MonoFunction { id; function_id; tyargs; body = None } in
  let env = Env { mods; methods; decls; monos = mono :: monos; exports; supply } in
  (env, id)

let add_instance_method_monomorph (env: env) (method_id: ins_meth_id) (tyargs: mono_type_bindings): (env * mono_id) =
  let (Env { mods; methods; decls; monos; exports; supply }) = env in
  let (id, supply) = fresh_mono_id supply in
  let mono = MonoInstanceMethod { id; method_id; tyargs; body = None } in
  let env = Env { mods; methods; decls; monos = mono :: monos; exports; supply } in
  (env, id)

let monomorph_id (mono: monomorph): mono_id =
  match mono with
  | MonoRecordDefinition { id; _ } -> id
  | MonoUnionDefinition { id; _ } -> id
  | MonoFunction { id; _ } -> id
  | MonoInstanceMethod { id; _ } -> id

let get_monomorph (env: env) (id: mono_id): monomorph =
  let (Env { monos; _ }) = env in
  registered (fun () -> "monomorph with ID `" ^ (show_mono_id id) ^ "`")
    (List.find_opt (fun mono -> equal_mono_id id (monomorph_id mono)) monos)

let find_type_monomorph (env: env) (decl_id: decl_id) (args: mono_type_bindings): mono_id option =
  let (Env { monos; _ }) = env in
  let pred (mono: monomorph): bool =
    match mono with
    | MonoRecordDefinition { type_id; tyargs; _ } ->
      (equal_decl_id type_id decl_id)
      && (equal_mono_bindings tyargs args)
    | MonoUnionDefinition { type_id; tyargs; _ } ->
      (equal_decl_id type_id decl_id)
      && (equal_mono_bindings tyargs args)
    | _ ->
      false
  in
  match List.find_opt pred monos with
  | Some m -> Some (monomorph_id m)
  | None -> None

let add_or_get_record_monomorph (env: env) (decl_id: decl_id) (args: mono_type_bindings): (env * mono_id) =
  match find_type_monomorph env decl_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_record_monomorph env decl_id args

let add_or_get_union_monomorph (env: env) (decl_id: decl_id) (args: mono_type_bindings): (env * mono_id) =
  match find_type_monomorph env decl_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_union_monomorph env decl_id args

let find_function_monomorph (env: env) (decl_id: decl_id) (args: mono_type_bindings): mono_id option =
  let (Env { monos; _ }) = env in
  let pred (mono: monomorph): bool =
    match mono with
    | MonoFunction { function_id; tyargs; _ } ->
      (equal_decl_id function_id decl_id)
      && (equal_mono_bindings tyargs args)
    | _ ->
      false
  in
  match List.find_opt pred monos with
  | Some m -> Some (monomorph_id m)
  | None -> None

let add_or_get_function_monomorph (env: env) (decl_id: decl_id) (args: mono_type_bindings): (env * mono_id) =
  match find_function_monomorph env decl_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_function_monomorph env decl_id args

(* Unlike an ID, a declaration and its type arguments can name a monomorph that
   is not registered yet: `add_or_get_*` register it the first time. The stages
   that call the next two functions (the entrypoint, and the wrappers of
   exported functions) run after monomorphization has registered what they ask
   for. *)
let get_type_monomorph (env: env) (decl_id: decl_id) (args: mono_type_bindings): mono_id =
  registered (fun () -> "monomorph of the type with ID `" ^ (show_decl_id decl_id) ^ "` and these type arguments")
    (find_type_monomorph env decl_id args)

let get_function_monomorph (env: env) (decl_id: decl_id) (args: mono_type_bindings): mono_id =
  registered (fun () -> "monomorph of the function with ID `" ^ (show_decl_id decl_id) ^ "` and these type arguments")
    (find_function_monomorph env decl_id args)

let find_instance_method_monomorph (env: env) (id: ins_meth_id) (args: mono_type_bindings): mono_id option =
  let (Env { monos; _ }) = env in
  let pred (mono: monomorph): bool =
    match mono with
    | MonoInstanceMethod { method_id; tyargs; _ } ->
      (equal_ins_meth_id method_id id)
      && (equal_mono_bindings tyargs args)
    | _ ->
      false
  in
  match List.find_opt pred monos with
  | Some m -> Some (monomorph_id m)
  | None -> None

let add_or_get_instance_method_monomorph (env: env) (method_id: ins_meth_id) (args: mono_type_bindings): (env * mono_id) =
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

let get_uninstantiated_monomorphs (env: env): monomorph list =
  let (Env { monos; _ }) = env in
  let pred (m: monomorph) =
    not (is_instantiated m)
  in
  List.rev (List.filter pred monos)

(** Find a monomorph by ID and return it, removing it from the
   environment. *)
let pop_monomorph (env: env) (id: mono_id): (env * monomorph) =
  let mono: monomorph = get_monomorph env id in
  let (Env { mods; methods; decls; monos; exports; supply }) = env in
  let other_monos = List.filter (fun m -> not (equal_mono_id (monomorph_id m) id)) monos in
  let env = Env { mods; methods; decls; monos = other_monos; exports; supply } in
  (env, mono)

let store_record_monomorph_definition (env: env) (id: mono_id) (new_slots: mono_slot list): env =
  let (env, mono) = pop_monomorph env id in
  match mono with
  | MonoRecordDefinition { id; type_id; tyargs; slots; } ->
    let _ = slots in
    let new_mono = MonoRecordDefinition { id; type_id; tyargs; slots = Some new_slots; } in
    let (Env { mods; methods; decls; monos; exports; supply }) = env in
    let env = Env { mods; methods; decls; monos = new_mono :: monos; exports; supply } in
    env
  | _ ->
    internal_err ("Couldn't find monomorph record definition with ID: " ^ (show_mono_id id))

let store_union_monomorph_definition (env: env) (id: mono_id) (new_cases: mono_case list): env =
  let (env, mono) = pop_monomorph env id in
  match mono with
  | MonoUnionDefinition { id; type_id; tyargs; cases; } ->
    let _ = cases in
    let new_mono = MonoUnionDefinition { id; type_id; tyargs; cases = Some new_cases; } in
    let (Env { mods; methods; decls; monos; exports; supply }) = env in
    let env = Env { mods; methods; decls; monos = new_mono :: monos; exports; supply } in
    env
  | _ ->
    internal_err ("Couldn't find monomorph union definition with ID: " ^ (show_mono_id id))

let store_function_monomorph_definition (env: env) (id: mono_id) (new_body: mstmt): env =
  let (env, mono) = pop_monomorph env id in
  match mono with
  | MonoFunction { id; function_id; tyargs; body; } ->
    let _ = body in
    let new_mono = MonoFunction { id; function_id; tyargs; body = Some new_body; } in
    let (Env { mods; methods; decls; monos; exports; supply }) = env in
    let env = Env { mods; methods; decls; monos = new_mono :: monos; exports; supply } in
    env
  | _ ->
    internal_err ("Couldn't find monomorph function definition with ID: " ^ (show_mono_id id))

let store_instance_method_monomorph_definition (env: env) (id: mono_id) (new_body: mstmt): env =
  let (env, mono) = pop_monomorph env id in
  match mono with
  | MonoInstanceMethod { id; method_id; tyargs; body; } ->
    let _ = body in
    let new_mono = MonoInstanceMethod { id; method_id; tyargs; body = Some new_body; } in
    let (Env { mods; methods; decls; monos; exports; supply }) = env in
    let env = Env { mods; methods; decls; monos = new_mono :: monos; exports; supply } in
    env
  | _ ->
    internal_err ("Couldn't find monomorph instance method definition with ID: " ^ (show_mono_id id))

let add_exported_function (env: env) (id: decl_id) (export_name: string): env =
  let (Env { mods; methods; decls; monos; exports; supply }) = env in
  let env = Env { mods; methods; decls; monos; exports = (id, export_name) :: exports; supply } in
  env

let get_export_functions (env: env): (decl_id * string) list =
  let (Env { exports; _ }) = env in
  exports

let with_supply (env: env) (stage: Supply.t -> ('a * Supply.t) Outcome.t): (env * 'a) Outcome.t =
  let (Env { mods; methods; decls; monos; exports; supply }) = env in
  Outcome.map
    (fun (result, supply) -> (Env { mods; methods; decls; monos; exports; supply }, result))
    (stage supply)
