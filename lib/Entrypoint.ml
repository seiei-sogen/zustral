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
open Monomorphize
open Stages.Mtast

(** The kind of entrypoint function we have, with the IDs of the types its
    signature names. *)
type entrypoint_kind =
  | EmptyEntrypoint of type_decl_id
  (** Zero-parameter entrypoint function, and the ID of `ExitCode`. *)
  | RootCapEntrypoint of type_decl_id * type_decl_id
  (** Single-parameter entrypoint function with a root capability argument,
      and the IDs of `RootCapability` and `ExitCode`. *)

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
                (match exit_code_type_id rt with
                 | Some exit_code ->
                    (id, EmptyEntrypoint exit_code)
                 | None ->
                    entrypoint_error NoSpan (WrongReturnType rt))
             | [ValueParameter (_, pt)] ->
                (* Single parameter case: the `root` parameter. *)
                (match root_cap_type_id pt with
                 | Some root_cap ->
                    (match exit_code_type_id rt with
                     | Some exit_code ->
                        (id, RootCapEntrypoint (root_cap, exit_code))
                     | None ->
                        entrypoint_error NoSpan (WrongReturnType rt))
                 | None ->
                    entrypoint_error NoSpan (WrongParameterType pt))
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

and root_cap_type_id = function
  | NamedType (name, id, [], LinearUniverse) ->
     let m = equal_module_name (source_module_name name) pervasive_module_name
     and n = equal_identifier (original_name name) (make_ident BuiltInNames.root_capability_name) in
     if m && n then Some id else None
  | NamedType (_, _, [], (FreeUniverse | TypeUniverse | RegionUniverse))
  | NamedType (_, _, _ :: _, _)
  | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _ | ReadRef _
  | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _ ->
     None

and exit_code_type_id = function
  | NamedType (name, id, [], FreeUniverse) ->
     let m = equal_module_name (source_module_name name) pervasive_module_name
     and n = equal_identifier (original_name name) (make_ident BuiltInNames.exit_code_name) in
     if m && n then Some id else None
  | NamedType (_, _, [], (LinearUniverse | TypeUniverse | RegionUniverse))
  | NamedType (_, _, _ :: _, _)
  | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _ | ReadRef _
  | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _ ->
     None

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

(* The monomorphs of the types the signature names are asked for as every
   use of a type asks. The monomorphization of the entrypoint's module
   registered them, so in a program the environment has them and nothing is
   instantiated here; the declarations of any it did not have come back to be
   generated. *)
let entrypoint_code (env: env) (name: qident): (env * mdecl list * CRepr.c_source) Outcome.t =
  Outcome.catch (fun () ->
      let (entrypoint_id, kind): function_id * entrypoint_kind = check_entrypoint_validity env name in
      let (env, code): env * string =
        match kind with
        | EmptyEntrypoint exit_code ->
           let (exit_code, env) = monomorphize_named_type env exit_code [] in
           (env, empty_entrypoint_code entrypoint_id exit_code)
        | RootCapEntrypoint (root_cap, exit_code) ->
           let (root_cap, env) = monomorphize_named_type env root_cap [] in
           let (exit_code, env) = monomorphize_named_type env exit_code [] in
           (env, entrypoint_code root_cap exit_code entrypoint_id)
      in
      let (env, decls): env * mdecl list = instantiate_monomorphs_until_exhausted env in
      (env, decls, CRepr.CSource code))
