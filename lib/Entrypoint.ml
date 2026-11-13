(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Id
open Identifier
open Env
open Type
open TypeParameters
open Error
open BuiltIn
open CodeGen
open MonoTypeBindings

(** The kind of entrypoint function we have. *)
type entrypoint_kind =
  | EmptyEntrypoint
  (** Zero-parameter entrypoint function. *)
  | RootCapEntrypoint
  (** Single-parameter entrypoint function with a root capability argument. *)

(** Check the entrypoint function is valid, return the ID of the function
    and the entrypoint kind. *)
let rec check_entrypoint_validity (env: env) (name: qident): function_id * entrypoint_kind =
  match get_decl_by_name NoSpan env (qident_to_sident name) with
  | Some decl ->
     (match decl with
      | Function { id; vis; typarams; value_params; rt; _ } ->
         let public: bool =
           match vis with
           | VisPublic -> true
           | VisPrivate -> false
         in
         if public then
           if (typarams_size typarams) = 0 then
             match value_params with
             | [] ->
               (* Empty parameter list case *)
                if is_exit_code_type rt then
                  (id, EmptyEntrypoint)
                else
                  entrypoint_error NoSpan (WrongReturnType rt)
             | [ValueParameter (_, pt)] ->
                (* Single parameter case: the `root` parameter. *)
                if is_root_cap_type pt then
                  if is_exit_code_type rt then
                    (id, RootCapEntrypoint)
                  else
                    entrypoint_error NoSpan (WrongReturnType rt)
                else
                  entrypoint_error NoSpan (WrongParameterType pt)
             | _ :: _ :: _ ->
                entrypoint_error NoSpan MultipleParameters
           else
             entrypoint_error NoSpan NotMonomorphic
         else
           entrypoint_error NoSpan NotPublic
      | Constant _ | Record _ | Union _ | UnionCase _ | TypeClass _ | TypeClassMethod _
      | Instance _ ->
         entrypoint_error NoSpan NotFunction)
  | None ->
      entrypoint_error NoSpan (NoSuchFunction {
          name = original_name name;
          module_name = source_module_name name;
        })

and is_root_cap_type = function
  | NamedType (name, [], LinearUniverse) ->
     let m = equal_module_name (source_module_name name) pervasive_module_name
     and n = equal_identifier (original_name name) (make_ident BuiltInNames.root_capability_name) in
     m && n
  | NamedType (_, [], (FreeUniverse | TypeUniverse | RegionUniverse))
  | NamedType (_, _ :: _, _)
  | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _ | ReadRef _
  | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
  | MonoTy _ ->
     false

and is_exit_code_type = function
  | NamedType (name, [], FreeUniverse) ->
     let m = equal_module_name (source_module_name name) pervasive_module_name
     and n = equal_identifier (original_name name) (make_ident BuiltInNames.exit_code_name) in
     m && n
  | NamedType (_, [], (LinearUniverse | TypeUniverse | RegionUniverse))
  | NamedType (_, _ :: _, _)
  | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _ | ReadRef _
  | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
  | MonoTy _ ->
     false

let entrypoint_code root_cap_mono_id exit_code_mono_id id =
  let f = CIdent.render (gen_function_id id) in
  let exit_code: string = CIdent.render (gen_mono_id exit_code_mono_id) in
  ("int main(int argc, char** argv) {\n"
   ^ "    au_store_cli_args(argc, argv);\n"
   ^ "    " ^ exit_code ^ " result = " ^ f ^ "((" ^ (CIdent.render (gen_mono_id root_cap_mono_id)) ^ "){ .value = false });\n"
   ^ "    switch(result.tag) {\n"
   ^ "        case " ^ exit_code ^ "_tag_" ^ BuiltInNames.exit_success_name ^ ":\n"
   ^ "            return 0;\n"
   ^ "        case " ^ exit_code ^ "_tag_" ^ BuiltInNames.exit_failure_name ^ ":\n"
   ^ "            return 1;\n"
   ^ "    }\n"
   ^ "}")

let empty_entrypoint_code (entrypoint_id: function_id) (exit_code_id: mono_id): string =
  let f = CIdent.render (gen_function_id entrypoint_id) in
  let exit_code: string = CIdent.render (gen_mono_id exit_code_id) in
  ("int main(int argc, char** argv) {\n"
   ^ "    au_store_cli_args(argc, argv);\n"
   ^ "    " ^ exit_code ^ " result = " ^ f ^ "();\n"
   ^ "    switch(result.tag) {\n"
   ^ "        case " ^ exit_code ^ "_tag_" ^ BuiltInNames.exit_success_name ^ ":\n"
   ^ "            return 0;\n"
   ^ "        case " ^ exit_code ^ "_tag_" ^ BuiltInNames.exit_failure_name ^ ":\n"
   ^ "            return 1;\n"
   ^ "    }\n"
   ^ "}")

let get_root_capability_monomorph (env: env): mono_id =
  let mn: module_name = pervasive_module_name
  and n: identifier = make_ident BuiltInNames.root_capability_name in
  let sn: sident = make_sident mn n in
  match get_decl_by_name NoSpan env sn with
  | Some (Record { id; _ }) ->
     (match get_record_monomorph env id empty_mono_bindings with
      | Some id ->
         mono_id_of_record id
      | None ->
         (* No type rules this out. The pass that monomorphized the signature
            of the entrypoint registered the monomorph under its key, and did
            not hand its ID over. *)
         internal_err NoSpan "No monomorph of RootCapability.")
  | None | Some (Constant _ | Union _ | UnionCase _ | Function _ | TypeClass _
      | TypeClassMethod _ | Instance _) ->
     (* No type rules this out. The compiler knows this type by the name the
        built-in module declares it under, and by nothing else. *)
     internal_err NoSpan "Can't find the RootCapability type in the environment."

let get_exit_code_monomorph (env: env): mono_id =
  let mn: module_name = pervasive_module_name
  and n: identifier = make_ident BuiltInNames.exit_code_name in
  let sn: sident = make_sident mn n in
  match get_decl_by_name NoSpan env sn with
  | Some (Union { id; _ }) ->
     (match get_union_monomorph env id empty_mono_bindings with
      | Some id ->
         mono_id_of_union id
      | None ->
         (* As for RootCapability: registered by key, by an earlier pass. *)
         internal_err NoSpan "No monomorph of ExitCode.")
  | None | Some (Constant _ | Record _ | UnionCase _ | Function _ | TypeClass _
      | TypeClassMethod _ | Instance _) ->
     (* As for RootCapability: known by its name only. *)
     internal_err NoSpan "Can't find the ExitCode type in the environment."

let entrypoint_code (env: env) (name: qident): CRepr.c_source Outcome.t =
  Outcome.catch (fun () ->
      let (entrypoint_id, kind): function_id * entrypoint_kind = check_entrypoint_validity env name in
      match kind with
      | EmptyEntrypoint ->
         CRepr.CSource (empty_entrypoint_code entrypoint_id (get_exit_code_monomorph env))
      | RootCapEntrypoint ->
         CRepr.CSource (entrypoint_code (get_root_capability_monomorph env) (get_exit_code_monomorph env) entrypoint_id))
