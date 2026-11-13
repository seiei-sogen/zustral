(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Common
open Type
open Error

type var_source =
  | VarConstant
  | VarParam
  | VarLocal of mutability

type lexenv = LexEnv of (identifier * ty * var_source) list

let empty_lexenv: lexenv =
  LexEnv []

let get_var (l: lexenv) (name: identifier): (ty * var_source) option =
  let (LexEnv inner) = l in
  Option.map (fun (_, t, s) -> (t, s)) (List.find_opt (fun (n, _, _) -> n = name) inner)

let get_temporary_type (place: place) (l: lexenv) (name: identifier): ty =
  match get_var l name with
  | Some (ty, _) ->
     ty
  | None ->
     (* Depends on the stage: the desugaring passes that make a temporary
        declare it before they use it, in the same block. The lexical
        environment does not tell temporaries from other variables. *)
     internal_err place ("Temporary variable does not exist in lexenv: " ^ (ident_string name))

let push_var (place: place) (l: lexenv) (name: identifier) (ty: ty) (source: var_source): lexenv =
  let (LexEnv inner) = l in
  match get_var l name with
  | (Some _) ->
     generic_error place (VariableAlreadyExists name)
  | None ->
     LexEnv ((name, ty, source) :: inner)

let rec push_vars (place: place) env l =
  match l with
  | (n,t,s)::rest ->
     push_vars place (push_var place env n t s) rest
  | [] ->
     env

let remove_var (l: lexenv) (name: identifier): lexenv =
  let (LexEnv inner) = l in
  let filtered =
    List.filter (fun (n, _, _) -> not (equal_identifier name n)) inner
  in
  LexEnv filtered

let rec remove_vars l names =
  match names with
  | [] ->
     l
  | first::rest ->
     remove_vars (remove_var l first) rest
