(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(* A catch-all arm over a variant type is an error in this file: a new
   constructor must stop the build at every match. *)
[@@@warning "@4"]

open Env
open Stages.Tast

let rec extract_bodies (env: env) (m: LinearityCheck.checked_module): env Outcome.t =
  let (TypedModule (_, decls)) = LinearityCheck.as_typed_module m in
  Outcome.catch (fun () -> Util.iter_with_context extract_decl env decls)

and extract_decl (env: env) (decl: typed_decl): env =
  match decl with
  | TFunction (decl_id, _, _, _, _, _, body, _) ->
      store_function_body env decl_id body
  | TInstance (_, _, _, _, _, methods, _) ->
     Util.iter_with_context extract_method env methods
  | TConstant _ | TRecord _ | TUnion _ | TForeignFunction _ | TTypeClass _ ->
     env

and extract_method (env: env) (meth: typed_method_def): env =
  let (TypedMethodDef (ins_meth_id, _, _, _, body)) = meth in
  store_method_body env ins_meth_id body
