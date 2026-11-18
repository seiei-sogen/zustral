(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Type
open TypeBindings
open TypeParameter
open TypeParameters
open EnvTypes
open Error
open DeclarationErrors

let check_instance_methods_match_typeclass
      (place: place)
      (typeclass_name: identifier)
      (typeclass_param: type_parameter)
      (argument: ty)
      (typeclass_methods: typeclass_method_decl list)
      (instance_methods: instance_method_input list): unit =
  let difference (tc_method: typeclass_method_decl) (ins_method: instance_method_input): function_difference option =
    let tc_typarams = typarams_as_list tc_method.typarams
    and ins_typarams = typarams_as_list ins_method.typarams in
    if List.length tc_typarams <> List.length ins_typarams then
      Some DifferentTypeParameters
    else
      (* The method's own type parameters are compared by position: an
         instance may name them differently, as it must when the typeclass's
         name is taken by a type parameter of the instance. *)
      let bindings: type_bindings =
        bindings_from_list place
          ((typeclass_param, argument)
           :: List.map2 (fun tc ins -> (tc, TyVar (typaram_to_tyvar ins))) tc_typarams ins_typarams)
      in
      let param_types (params: value_parameter list): ty list =
        List.map (fun (ValueParameter (_, ty)) -> ty) params
      in
      let expected_params: ty list = List.map (replace_variables bindings) (param_types tc_method.value_params) in
      if not (List.equal equal_ty expected_params (param_types ins_method.value_params)) then
        Some DifferentValueParameters
      else if not (equal_ty (replace_variables bindings tc_method.rt) ins_method.rt) then
        Some DifferentReturnTypes
      else
        None
  in
  let check_method (tc_method: typeclass_method_decl): unit =
    let method_name: identifier = tc_method.name in
    let definitions: instance_method_input list =
      List.filter (fun (m: instance_method_input) -> equal_identifier m.name method_name) instance_methods
    in
    match definitions with
    | [] ->
       declaration_error place (InstanceMissingMethod { typeclass = typeclass_name; ty = argument; method_name })
    | [ins_method] ->
       (match difference tc_method ins_method with
        | Some difference ->
           declaration_error place (InstanceMethodMismatch { typeclass = typeclass_name; ty = argument; method_name; difference })
        | None ->
           ())
    | _ :: _ :: _ ->
       declaration_error place (InstanceMethodDefinedTwice { typeclass = typeclass_name; ty = argument; method_name })
  in
  List.iter check_method typeclass_methods
