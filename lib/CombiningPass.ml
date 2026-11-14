(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Imports
open Qualifier
open TypeParameter
open TypeParameters
open AbstractionPass
open Cst
open CstUtil
open Stages.Combined
open Env
open ImportResolution
open Error

let parse_slots (imports: import_map) (slots: concrete_slot list): qslot list =
  List.map
    (fun (ConcreteSlot (n, t)) -> QualifiedSlot (n, qualify_typespec imports t))
    slots

let parse_cases (imports: import_map) (cases: concrete_case list): qcase list =
  List.map
    (fun (ConcreteCase (n, slots)) ->
      QualifiedCase (n,
                     List.map (fun (ConcreteSlot (n, t)) ->
                         QualifiedSlot (n, qualify_typespec imports t))
                       slots))
    cases

let parse_params (imports: import_map) (params: concrete_param list): qparam list =
  List.map
    (fun (ConcreteParam (n, t)) ->
      QualifiedParameter (n, qualify_typespec imports t))
    params

let name_typaram (im: import_map) (name: qident) (ConcreteTypeParam (n, u, cs): concrete_type_param): type_parameter =
  make_typaram (n, u, name, List.map (fun i -> qident_to_sident (qualify_identifier im i)) cs)

let name_typarams (place: place) (im: import_map) (params: concrete_type_param list) (name: qident): typarams =
  typarams_from_list place (List.map (name_typaram im name) params)

let parse_method_decls (module_name: module_name) (imports: import_map) (methods: concrete_method_decl list): combined_method_decl list =
  List.map (fun (ConcreteMethodDecl (n, typarams, params, rt, method_docstring)) ->
      CMethodDecl (n,
                   name_typarams NoSpan imports typarams (make_qident (module_name, n, n)),
                   parse_params imports params,
                   qualify_typespec imports rt,
                   method_docstring))
    methods

let parse_method_defs (place: place) (module_name: module_name) (imports: import_map) (methods: concrete_method_def list): combined_method_def list =
  List.map (fun (ConcreteMethodDef (n, typarams, params, rt, body, method_docstring)) ->
      CMethodDef (n,
                  name_typarams place imports typarams (make_qident (module_name, n, n)),
                  parse_params imports params,
                  qualify_typespec imports rt,
                  method_docstring,
                  abs_stmt place imports body))
    methods

(* The declarations of the interface that need a definition in the body, each
   with the definition of that name. *)
let match_constant (ii: import_map) (bi: import_map) (span: Span.span) (name: identifier) (ty: typespec) (docstring: Common.docstring) (def: concrete_def): combined_definition =
  match def with
  | ConcreteConstantDef (defspan, name', ty', value, _) ->
     if (name = name') && (ty = ty') then
       CConstant (defspan, VisPublic, name, qualify_typespec ii ty, abs_expr (AtSpan span) bi value, docstring)
     else
       declaration_error (AtSpan span) (DeclarationTypeMismatch name)
  | ConcreteRecordDef _ | ConcreteUnionDef _ | ConcreteFunctionDef _
  | ConcreteTypeClassDef _ | ConcreteInstanceDef _ ->
     declaration_error (AtSpan span) (DeclarationKindMismatch { name; expected = ConstantDeclaration })

let match_opaque_type (module_name: module_name) (bi: import_map) (span: Span.span) (name: identifier) (typarams: concrete_type_param list) (universe: Type.universe) (docstring: Common.docstring) (def: concrete_def): combined_definition =
  let make_qname n =
    make_qident (module_name, n, n)
  in
  match def with
  | ConcreteRecordDef (ConcreteRecord (defspan, name', typarams', universe', slots, _)) ->
     if (name = name') && (typarams = typarams') && (universe = universe') then
       let qname = make_qname name' in
       CRecord (defspan,
                TypeVisOpaque,
                name,
                name_typarams (AtSpan span) bi typarams qname,
                universe,
                parse_slots bi slots,
                docstring)
     else
       if universe != universe' then
         declaration_error (AtSpan span) (DeclarationUniverseMismatch name)
       else
         declaration_error (AtSpan span) (DeclarationTypeMismatch name)
  | ConcreteUnionDef (ConcreteUnion (defspan, name', typarams', universe', cases, _)) ->
     if (name = name') && (typarams = typarams') && (universe = universe') then
       let qname = make_qname name' in
       CUnion (defspan,
               TypeVisOpaque,
               name,
               name_typarams (AtSpan span) bi typarams qname,
               universe,
               parse_cases bi cases,
               docstring)
     else
       if universe <> universe' then
         declaration_error (AtSpan span) (DeclarationUniverseMismatch name)
       else
         declaration_error (AtSpan span) (DeclarationTypeMismatch name)
  | ConcreteConstantDef _ | ConcreteFunctionDef _ | ConcreteTypeClassDef _
  | ConcreteInstanceDef _ ->
     declaration_error (AtSpan span) (DeclarationKindMismatch { name; expected = TypeDeclaration })

(* The definition has the declaration's name: get_concrete_def found it by
   that name. *)
let match_function (module_name: module_name) (ii: import_map) (bi: import_map) (span: Span.span) (name: identifier) (typarams: concrete_type_param list) (params: concrete_param list) (rt: typespec) (docstring: Common.docstring) (def: concrete_def): combined_definition =
  match def with
  | ConcreteFunctionDef (defspan, name', typarams', params', rt', body, _, pragmas) ->
     if (typarams = typarams') && (params = params') && (rt = rt') then
       let qname = make_qident (module_name, name', name') in
       CFunction (defspan,
                  VisPublic,
                  name,
                  name_typarams (AtSpan span) bi typarams qname,
                  parse_params ii params,
                  qualify_typespec ii rt,
                  abs_stmt (AtSpan span) bi body,
                  docstring,
                  pragmas)
     else
       let difference: DeclarationErrors.function_difference =
         if typarams <> typarams' then
           DifferentTypeParameters
         else if params <> params' then
           DifferentValueParameters
         else
           DifferentReturnTypes
       in
       declaration_error (AtSpan span) (FunctionMismatch { name; difference })
  | ConcreteConstantDef _ | ConcreteRecordDef _ | ConcreteUnionDef _
  | ConcreteTypeClassDef _ | ConcreteInstanceDef _ ->
     declaration_error (AtSpan span) (DeclarationKindMismatch { name; expected = FunctionDeclaration })

(* The definition is the one get_instance_def found by the declaration's name,
   type parameters and argument. *)
let match_instance (module_name: module_name) (ii: import_map) (bi: import_map) (span: Span.span) (name: identifier) (typarams: concrete_type_param list) (argument: typespec) (docstring: Common.docstring) (ConcreteInstance (defspan, _, _, _, methods, _)): combined_definition =
  (* Instance names might refer to an imported typeclass, so we have to
     qualify them. Since we're parsing a public declaration, which
     means the instance (and thus the name of the typeclass) appears in
     the interface file, we use the interface imports for
     qualification. *)
  let qname = qualify_identifier ii name in
  CInstance (defspan,
             span,
             VisPublic,
             qname,
             name_typarams (AtSpan span) bi typarams qname,
             qualify_typespec ii argument,
             parse_method_defs (AtSpan span) module_name bi methods,
             docstring)

let private_def module_name im def =
  let make_qname n =
    make_qident (module_name, n, n)
  in
  match def with
  | ConcreteConstantDef (span, name, ty, value, docstring) ->
     CConstant (span,
                VisPrivate,
                name,
                qualify_typespec im ty,
                abs_expr NoSpan im value,
                docstring)
  | ConcreteRecordDef (ConcreteRecord (span, name, typarams, universe, slots, docstring)) ->
     let qname = make_qname name in
     CRecord (span,
              TypeVisPrivate,
              name,
              name_typarams NoSpan im typarams qname,
              universe,
              parse_slots im slots,
              docstring)
  | ConcreteUnionDef (ConcreteUnion (span, name, typarams, universe, cases, docstring)) ->
     let qname = make_qname name in
     CUnion (span,
             TypeVisPrivate,
             name,
             name_typarams NoSpan im typarams qname,
             universe,
             parse_cases im cases,
             docstring)
  | ConcreteFunctionDef (span, name, typarams, params, rt, body, docstring, pragmas) ->
     let qname = make_qname name in
     CFunction (span,
                VisPrivate,
                name,
                name_typarams NoSpan im typarams qname,
                parse_params im params,
                qualify_typespec im rt,
                abs_stmt NoSpan im body,
                docstring,
                pragmas)
  | ConcreteTypeClassDef (ConcreteTypeClass (span, name, typaram, methods, docstring)) ->
     let qname = make_qname name in
     CTypeclass (span,
                 VisPrivate,
                 name,
                 name_typaram im qname typaram,
                 parse_method_decls module_name im methods,
                 docstring)
  | ConcreteInstanceDef (ConcreteInstance (span, name, typarams, argument, methods, docstring)) ->
     (* Instance names might refer to an imported typeclass, so we have to
        qualify them. Since we're parsing a private declaration, which means the
        instance (and thus the name of the typeclass) appears in the body file,
        we can use the body imports for qualification. *)
     let qname = qualify_identifier im name in
     CInstance (span,
                span,
                VisPrivate,
                qname,
                name_typarams NoSpan im typarams qname,
                qualify_typespec im argument,
                parse_method_defs NoSpan module_name im methods,
                docstring)

open Outcome.Syntax

let rec combine (env: env) (cmi: concrete_module_interface) (cmb: concrete_module_body): combined_module Outcome.t =
  let (ConcreteModuleInterface (mn, interface_docstring, interface_imports, decls)) = cmi
  and (ConcreteModuleBody (mn', kind, body_docstring, body_imports, defs)) = cmb
  in
  let* (im, bm) =
    Outcome.catch (fun () ->
        if mn <> mn' then
          declaration_error NoSpan (ModuleNameMismatch { interface = mn; body = mn' })
        else
          let im = resolve mn kind env interface_imports
          and bm = resolve mn kind env body_imports
          in
          (im, bm))
  in
  let* public_decls = Outcome.traverse (fun decl -> Outcome.catch (fun () -> parse_decl mn im bm cmb decl)) decls in
  let* private_decls = parse_defs mn cmi bm defs in
  Ok (CombinedModule {
          name = mn;
          kind = kind;
          interface_docstring = interface_docstring;
          interface_imports = im;
          body_docstring = body_docstring;
          body_imports = bm;
          decls = List.concat [public_decls; private_decls];
    })

and parse_decl (module_name: module_name) (im: import_map) (bm: import_map) (cmb: concrete_module_body) (decl: concrete_decl): combined_definition =
  let make_qname n =
    make_qident (module_name, n, n)
  in
  (* Some declarations need a matching definition in the body. *)
  let definition (name: identifier): concrete_def =
    match get_concrete_def cmb name with
    | (Some def) ->
       def
    | None ->
       declaration_error NoSpan (MissingBodyDefinition name)
  in
  match decl with
  | ConcreteInstanceDecl (span, name, typarams, argument, docstring) ->
     (* It's an instance declaration. Find the corresponding instance in the body. *)
     (match get_instance_def cmb name typarams argument with
      | (Some def) ->
         match_instance module_name im bm span name typarams argument docstring def
      | None ->
         declaration_error NoSpan (MissingInstanceBodyDefinition { name; ty = WrittenType (typespec_string argument) }))
  | ConcreteConstantDecl (span, name, ty, docstring) ->
     match_constant im bm span name ty docstring (definition name)
  | ConcreteOpaqueTypeDecl (span, name, typarams, universe, docstring) ->
     match_opaque_type module_name bm span name typarams universe docstring (definition name)
  | ConcreteFunctionDecl (span, name, typarams, params, rt, docstring) ->
     match_function module_name im bm span name typarams params rt docstring (definition name)
  (* Some declarations don't need to have a matching body *)
  | ConcreteRecordDecl (ConcreteRecord (span, name, typarams, universe, slots, docstring)) ->
     let qname = make_qname name in
     CRecord (span,
              TypeVisPublic,
              name,
              name_typarams NoSpan im typarams qname,
              universe,
              parse_slots im slots,
              docstring)
  | ConcreteUnionDecl (ConcreteUnion (span, name, typarams, universe, cases, docstring)) ->
     let qname = make_qname name in
     CUnion (span,
             TypeVisPublic,
             name,
             name_typarams NoSpan im typarams qname,
             universe,
             parse_cases im cases,
             docstring)
  | ConcreteTypeClassDecl (ConcreteTypeClass (span, name, typaram, methods, docstring)) ->
     let qname = make_qname name in
     CTypeclass (span,
                 VisPublic,
                 name,
                 name_typaram im qname typaram,
                 parse_method_decls module_name im methods,
                 docstring)

and parse_defs (mn: module_name) (cmi: concrete_module_interface) (im: import_map) (defs: concrete_def list): combined_definition list Outcome.t =
  Outcome.map (List.filter_map (fun def -> def))
    (Outcome.traverse (fun def -> Outcome.catch (fun () -> parse_def mn cmi im def)) defs)

and parse_def (module_name: module_name) (cmi: concrete_module_interface) (im: import_map) (def: concrete_def): combined_definition option =
  match def with
  | ConcreteConstantDef (_, name, _, _, _) | ConcreteRecordDef (ConcreteRecord (_, name, _, _, _, _))
  | ConcreteUnionDef (ConcreteUnion (_, name, _, _, _, _)) | ConcreteFunctionDef (_, name, _, _, _, _, _, _)
  | ConcreteTypeClassDef (ConcreteTypeClass (_, name, _, _, _)) ->
     (* If this def exists in the interface, skip it *)
     (match get_concrete_decl cmi name with
      | (Some _) ->
         None
      | None ->
         Some (private_def module_name im def))
  | ConcreteInstanceDef (ConcreteInstance (_, name, typarams, argument, _, _)) ->
     (* It's an instance declaration. If the interface file declares it, ignore
        it: it's already been processed. Otherwise, process it as a private
        instance declaration. *)
     if has_instance_decl cmi name typarams argument then
       None
     else
       Some (private_def module_name im def)

let as_public (def: combined_definition): combined_definition =
  match def with
  | CConstant (span, _, name, ty, value, docstring) ->
     CConstant (span, VisPublic, name, ty, value, docstring)
  | CRecord (span, _, name, typarams, universe, slots, docstring) ->
     CRecord (span, TypeVisPublic, name, typarams, universe, slots, docstring)
  | CUnion (span, _, name, typarams, universe, cases, docstring) ->
     CUnion (span, TypeVisPublic, name, typarams, universe, cases, docstring)
  | CFunction (span, _, name, typarams, params, rt, body, docstring, pragmas) ->
     CFunction (span, VisPublic, name, typarams, params, rt, body, docstring, pragmas)
  | CTypeclass (span, _, name, typaram, methods, docstring) ->
     CTypeclass (span, VisPublic, name, typaram, methods, docstring)
  | CInstance (span, decl_span, _, name, typarams, argument, methods, docstring) ->
     CInstance (span, decl_span, VisPublic, name, typarams, argument, methods, docstring)

let body_as_combined (env: env) (body: concrete_module_body): combined_module Outcome.t =
  let (ConcreteModuleBody (mn, kind, body_docstring, body_imports, defs)) = body in
  (* We fake an empty module interface. *)
  let cmi: concrete_module_interface = ConcreteModuleInterface (mn, Docstring "", [], []) in
  let* imports = Outcome.catch (fun () -> resolve mn kind env body_imports) in
  let* decls = parse_defs mn cmi imports defs in
  (* Go through the declarations and make them public. *)
  let decls = List.map as_public decls in
  Ok (CombinedModule {
          name = mn;
          kind = kind;
          interface_docstring = Docstring "";
          interface_imports = empty_map mn;
          body_docstring = body_docstring;
          body_imports = imports;
          decls = decls;
    })
