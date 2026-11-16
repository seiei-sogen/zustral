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
  | VarParam
  | VarLocal of mutability

type variable_source =
  | VarConstant
  | VarLexical of var_source

(* The variables in scope, and the names of those whose `let` has not assigned
   its initial value yet. Such a variable is declared, so its name is taken,
   and has no value, so nothing can name it: not an expression, and not a
   borrow. *)
type lexenv = LexEnv of (identifier * ty * var_source) list * identifier list

let empty_lexenv: lexenv =
  LexEnv ([], [])

let get_declared_var (l: lexenv) (name: identifier): (ty * var_source) option =
  let (LexEnv (declared, _)) = l in
  Option.map (fun (_, t, s) -> (t, s)) (List.find_opt (fun (n, _, _) -> n = name) declared)

let get_var (l: lexenv) (name: identifier): (ty * var_source) option =
  let (LexEnv (_, awaiting_initial_value)) = l in
  if List.exists (equal_identifier name) awaiting_initial_value then
    None
  else
    get_declared_var l name

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
  let (LexEnv (declared, awaiting_initial_value)) = l in
  match get_declared_var l name with
  | (Some _) ->
     generic_error place (VariableAlreadyExists name)
  | None ->
     LexEnv ((name, ty, source) :: declared, awaiting_initial_value)

let push_var_awaiting_initial_value (place: place) (l: lexenv) (name: identifier) (ty: ty) (source: var_source): lexenv =
  let (LexEnv (declared, awaiting_initial_value)) = push_var place l name ty source in
  LexEnv (declared, name :: awaiting_initial_value)

let mark_var_initialized (l: lexenv) (name: identifier): lexenv =
  let (LexEnv (declared, awaiting_initial_value)) = l in
  LexEnv (declared, List.filter (fun n -> not (equal_identifier name n)) awaiting_initial_value)

let rec push_vars (place: place) env l =
  match l with
  | (n,t,s)::rest ->
     push_vars place (push_var place env n t s) rest
  | [] ->
     env

let remove_var (l: lexenv) (name: identifier): lexenv =
  let (LexEnv (declared, awaiting_initial_value)) = l in
  let filtered =
    List.filter (fun (n, _, _) -> not (equal_identifier name n)) declared
  in
  LexEnv (filtered, awaiting_initial_value)

let rec remove_vars l names =
  match names with
  | [] ->
     l
  | first::rest ->
     remove_vars (remove_var l first) rest
