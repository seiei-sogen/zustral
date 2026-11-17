(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Common
open Cst
open Error

(* A function definition takes the pragmas Foreign_Import and Foreign_Export,
   and a module body takes Unsafe_Module. A pragma anywhere else would do
   nothing, so it is an error (#93, #144). *)
let pragmas_must_be_absent (span: Span.span) (pragmas: pragma list): unit =
  match pragmas with
  | [] ->
     ()
  | ForeignImportPragma _ :: _ ->
     declaration_error (AtSpan span) (MisplacedPragma ForeignImport)
  | ForeignExportPragma _ :: _ ->
     declaration_error (AtSpan span) (MisplacedPragma ForeignExport)
  | UnsafeModulePragma :: _ ->
     declaration_error (AtSpan span) (MisplacedPragma UnsafeModule)

(* [pragmas_span] covers the pragmas written before `module body`. *)
let make_module_body (pragmas_span: Span.span) (name: module_name) (imports: concrete_import_list list) (pragmas: pragma list) (defs: concrete_def list) (docstring: docstring) =
  pragmas_must_be_absent pragmas_span (List.filter (fun p -> p <> UnsafeModulePragma) pragmas);
  let kind = if List.mem UnsafeModulePragma pragmas then UnsafeModule else SafeModule in
  ConcreteModuleBody (name, kind, docstring, imports, defs)

let concrete_decl_name = function
  | ConcreteConstantDecl (_, n, _, _) -> Some n
  | ConcreteOpaqueTypeDecl (_, n, _, _, _) -> Some n
  | ConcreteRecordDecl (ConcreteRecord (_, n, _, _, _, _)) -> Some n
  | ConcreteUnionDecl (ConcreteUnion (_, n, _, _, _, _)) -> Some n
  | ConcreteFunctionDecl (_, n, _, _, _, _) -> Some n
  | ConcreteTypeClassDecl (ConcreteTypeClass (_, n, _, _, _)) -> Some n
  | ConcreteInstanceDecl _ -> None

let def_name (def: concrete_named_def): identifier =
  match def with
  | ConcreteConstantDef (_, n, _, _, _) -> n
  | ConcreteRecordDef (ConcreteRecord (_, n, _, _, _, _)) -> n
  | ConcreteUnionDef (ConcreteUnion (_, n, _, _, _, _)) -> n
  | ConcreteFunctionDef (_, n, _, _, _, _, _, _) -> n
  | ConcreteTypeClassDef (ConcreteTypeClass (_, n, _, _, _)) -> n

let def_span (def: concrete_named_def): Span.span =
  match def with
  | ConcreteConstantDef (span, _, _, _, _) -> span
  | ConcreteRecordDef (ConcreteRecord (span, _, _, _, _, _)) -> span
  | ConcreteUnionDef (ConcreteUnion (span, _, _, _, _, _)) -> span
  | ConcreteFunctionDef (span, _, _, _, _, _, _, _) -> span
  | ConcreteTypeClassDef (ConcreteTypeClass (span, _, _, _, _)) -> span

let get_concrete_decl (ConcreteModuleInterface (_, _, _, decls)) name =
  let pred decl =
    match concrete_decl_name decl with
    | (Some name') ->
       name = name'
    | None ->
       false
  in
  List.find_opt pred decls

let get_concrete_def (ConcreteModuleBody (_, _, _, _, defs)) name =
  let pick (def: concrete_def): concrete_named_def option =
    match def with
    | ConcreteNamedDef named ->
       if def_name named = name then Some named else None
    | ConcreteInstanceDef _ ->
       None
  in
  List.find_map pick defs

let has_instance_decl (ConcreteModuleInterface (_, _, _, decls)) (name: identifier) (typarams: concrete_type_param list) (ty: typespec): bool =
  let pred = function
    | ConcreteInstanceDecl (_, name', typarams', ty', _) ->
       (name = name') && (typarams = typarams') && (ty = ty')
    | ConcreteConstantDecl _ | ConcreteOpaqueTypeDecl _ | ConcreteRecordDecl _
    | ConcreteUnionDecl _ | ConcreteFunctionDecl _ | ConcreteTypeClassDecl _ ->
       false
  in
  List.exists pred decls

let get_instance_def (ConcreteModuleBody (_, _, _, _, defs)) (name: identifier) (typarams: concrete_type_param list) (ty: typespec): concrete_instance option =
  let filter = function
    | ConcreteInstanceDef ci -> Some ci
    | ConcreteNamedDef _ -> None
  and pred (ConcreteInstance (_, name', typarams', ty', _, _)) =
    (name = name') && (typarams = typarams') && (ty = ty')
  in
  List.find_opt pred (List.filter_map filter defs)

let make_pragma name args =
  let s = ident_string name in
  if s = BuiltInNames.foreign_import_pragma_name then
    let raise_err () =
      declaration_error NoSpan (PragmaArgumentError ForeignImport)
    in
    match args with
    | ConcreteNamedArgs [(a, CStringConstant (_, f))] ->
       if equal_identifier a (make_ident BuiltInNames.external_name_argument) then
         ForeignImportPragma f
       else
         raise_err ()
    | ConcretePositionalArgs _ | ConcreteNamedArgs ([] | [(_, (CNilConstant _
        | CBoolConstant _ | CIntConstant _ | CFloatConstant _ | CVariable _ | CArith _
        | CArithmeticNegation _ | CFuncall _ | CComparison _ | CConjunction _ | CDisjunction _
        | CNegation _ | CIfExpression _ | CPath _ | CRefPath _ | CEmbed _ | CDeref _ | CTypecast _
        | CSizeOf _ | CBorrowExpr _ | CReborrow _))] | _ :: _ :: _) ->
       raise_err ()
  else
    if s = BuiltInNames.foreign_export_pragma_name then
      let raise_err () =
        declaration_error NoSpan (PragmaArgumentError ForeignExport)
      in
      match args with
      | ConcreteNamedArgs [(a, CStringConstant (_, f))] ->
         if equal_identifier a (make_ident BuiltInNames.external_name_argument) then
           ForeignExportPragma f
         else
           raise_err ()
      | ConcretePositionalArgs _ | ConcreteNamedArgs ([] | [(_, (CNilConstant _
          | CBoolConstant _ | CIntConstant _ | CFloatConstant _ | CVariable _
          | CArith _ | CArithmeticNegation _ | CFuncall _ | CComparison _ | CConjunction _
          | CDisjunction _ | CNegation _ | CIfExpression _ | CPath _ | CRefPath _ | CEmbed _ | CDeref _
          | CTypecast _ | CSizeOf _ | CBorrowExpr _ | CReborrow _))] | _ :: _ :: _) ->
         raise_err ()
    else
      if s = BuiltInNames.unsafe_module_pragma_name then
        let raise_err () =
          declaration_error NoSpan (PragmaArgumentError UnsafeModule)
        in
        match args with
        | ConcretePositionalArgs [] ->
           UnsafeModulePragma
        | ConcretePositionalArgs _ | ConcreteNamedArgs _ ->
           raise_err ()
      else
        declaration_error NoSpan (UnknownPragma name)

let mod_int_name (inter: concrete_module_interface): module_name =
  let (ConcreteModuleInterface (name, _, _, _)) = inter in
  name

let mod_body_name (body: concrete_module_body): module_name =
  let (ConcreteModuleBody (name, _, _, _, _)) = body in
  name

(* An interface declaration takes no pragma: a function's pragmas are written
   before its definition in the body. *)
let decl_replace_docstring_and_pragmas (decl: concrete_decl) (docstring: docstring) (pragmas: pragma list): concrete_decl =
  match decl with
  | ConcreteConstantDecl (span, name, ty, _) ->
     pragmas_must_be_absent span pragmas;
     ConcreteConstantDecl (span, name, ty, docstring)
  | ConcreteOpaqueTypeDecl (span, name, typarams, universe, _) ->
     pragmas_must_be_absent span pragmas;
     ConcreteOpaqueTypeDecl (span, name, typarams, universe, docstring)
  | ConcreteRecordDecl (ConcreteRecord (span, name, typarams, universe, slots, _)) ->
     pragmas_must_be_absent span pragmas;
     ConcreteRecordDecl (ConcreteRecord (span, name, typarams, universe, slots, docstring))
  | ConcreteUnionDecl (ConcreteUnion (span, name, typarams, universe, cases, _)) ->
     pragmas_must_be_absent span pragmas;
     ConcreteUnionDecl (ConcreteUnion (span, name, typarams, universe, cases, docstring))
  | ConcreteFunctionDecl (span, name, typarams, params, rt, _) ->
     pragmas_must_be_absent span pragmas;
     ConcreteFunctionDecl (span, name, typarams, params, rt, docstring)
  | ConcreteTypeClassDecl (ConcreteTypeClass (span, name, typarams, methods, _)) ->
     pragmas_must_be_absent span pragmas;
     ConcreteTypeClassDecl (ConcreteTypeClass (span, name, typarams, methods, docstring))
  | ConcreteInstanceDecl (span, name, methods, ty, _) ->
     pragmas_must_be_absent span pragmas;
     ConcreteInstanceDecl (span, name, methods, ty, docstring)

(* Of the definitions of a body, only a function takes pragmas. *)
let def_replace_docstring_and_pragmas (decl: concrete_def) (docstring: docstring) (pragmas: pragma list): concrete_def =
  match decl with
  | ConcreteNamedDef (ConcreteConstantDef (span, name, ty, expr, _)) ->
     pragmas_must_be_absent span pragmas;
     ConcreteNamedDef (ConcreteConstantDef (span, name, ty, expr, docstring))
  | ConcreteNamedDef (ConcreteRecordDef (ConcreteRecord (span, name, typarams, universe, slots, _))) ->
     pragmas_must_be_absent span pragmas;
     ConcreteNamedDef (ConcreteRecordDef (ConcreteRecord (span, name, typarams, universe, slots, docstring)))
  | ConcreteNamedDef (ConcreteUnionDef (ConcreteUnion (span, name, typarams, universe, cases, _))) ->
     pragmas_must_be_absent span pragmas;
     ConcreteNamedDef (ConcreteUnionDef (ConcreteUnion (span, name, typarams, universe, cases, docstring)))
  | ConcreteNamedDef (ConcreteFunctionDef (span, name, typarams, params, ty, body, _, _)) ->
     ConcreteNamedDef (ConcreteFunctionDef (span, name, typarams, params, ty, body, docstring, pragmas))
  | ConcreteNamedDef (ConcreteTypeClassDef (ConcreteTypeClass (span, name, typarams, methods, _))) ->
     pragmas_must_be_absent span pragmas;
     ConcreteNamedDef (ConcreteTypeClassDef (ConcreteTypeClass (span, name, typarams, methods, docstring)))
  | ConcreteInstanceDef (ConcreteInstance (span, name, typarams, ty, methods, _)) ->
     pragmas_must_be_absent span pragmas;
     ConcreteInstanceDef (ConcreteInstance (span, name, typarams, ty, methods, docstring))
