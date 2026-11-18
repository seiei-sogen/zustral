(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Stages.Tast
open EvaluationOrderOfStatement

type ordered_module = OrderedModule of typed_module

let fix_evaluation_order_of_module (checked: LinearityCheck.checked_module): ordered_module =
  let (TypedModule (module_name, decls)) = LinearityCheck.as_typed_module checked in
  let fix_method (TypedMethodDef (id, name, params, rt, body)): typed_method_def =
    TypedMethodDef (id, name, params, rt, fix_evaluation_order_of_statement body)
  in
  let fix_decl (decl: typed_decl): typed_decl =
    match decl with
    | TFunction (id, vis, name, typarams, params, rt, body, docstring) ->
       TFunction (id, vis, name, typarams, params, rt, fix_evaluation_order_of_statement body, docstring)
    | TInstance (id, vis, name, typarams, argument, methods, docstring) ->
       TInstance (id, vis, name, typarams, argument, List.map fix_method methods, docstring)
    | TConstant _ | TRecord _ | TUnion _ | TForeignFunction _ | TTypeClass _ ->
       decl
  in
  OrderedModule (TypedModule (module_name, List.map fix_decl decls))

let as_typed_module (OrderedModule m): typed_module =
  m
