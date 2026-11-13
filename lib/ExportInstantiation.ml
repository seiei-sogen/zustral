(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Id
open Env
open Type
open Error
open CodeGen
open CRepr

let rec transform_ty (ty: ty): c_ty =
  match ty with
  | Unit ->
     type_error (ExportDisallowedType ty)
  | Boolean ->
     CBuiltinType CBoolType
  | Integer (s, w) ->
     gen_integer_type s w
  | SingleFloat ->
     CBuiltinType CFloatType
  | DoubleFloat ->
     CBuiltinType CDoubleType
  | NamedType _ ->
     type_error (ExportDisallowedType ty)
  | RegionTy _ ->
     type_error (ExportDisallowedType ty)
  | ReadRef _ ->
     type_error (ExportDisallowedType ty)
  | WriteRef _ ->
     type_error (ExportDisallowedType ty)
  | Span _ ->
     type_error (ExportDisallowedType ty)
  | SpanMut _ ->
     type_error (ExportDisallowedType ty)
  | TyVar _ ->
     type_error (ExportDisallowedType ty)
  | Address t ->
     CPointer (transform_ty t)
  | Pointer _ ->
     type_error (ExportDisallowedType ty)
  | FnPtr _ ->
     fn_type
  | MonoTy _ ->
     type_error (ExportDisallowedType ty)

let transform_param (ValueParameter (name, ty)): c_param =
  CValueParam (gen_ident name, transform_ty ty)

let make_wrapper (env: env) ((function_id, export_name, mono_id): function_id * string * mono_id): c_decl =
  let { value_params = params; rt; _ }: EnvTypes.function_decl = get_function env function_id in
  CFunctionDefinition (
      Desc "Wrapper function",
      (* The name the user asked for gets the reserved-word escape (issue #92). *)
      CIdent.external_escaped export_name,
      List.map transform_param params,
      transform_ty rt,
      CBlock [
          CReturn (CFuncall (gen_mono_id mono_id, List.map (fun (ValueParameter (n, _)) -> CVar (gen_ident n)) params))
      ]
    )

let all_wrappers (env: env) (exported: (function_id * string * mono_id) list): c_decl list Outcome.t =
  Outcome.catch (fun () -> List.map (make_wrapper env) exported)
