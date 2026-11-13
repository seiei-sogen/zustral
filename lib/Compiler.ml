(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Env
open BuiltIn
open BuiltInModules
open ParserInterface
open CombiningPass
open ExtractionPass
open TypingPass
open BodyExtractionPass
open CodeGen
open CRepr
open Cst
open CstUtil
open Stages.Tast
open Error
open Stages
open Linked
open Mtast
open Monomorphize
open ReturnCheck
open LinearityCheck
open Entrypoint
open ExportInstantiation

let append_import_to_interface (ci: concrete_module_interface) (import: concrete_import_list): concrete_module_interface =
  let (ConcreteModuleInterface (mn, docstring, imports, decls)) = ci in
  if equal_module_name mn pervasive_module_name then
    ci
  else
    ConcreteModuleInterface (mn, docstring, import :: imports, decls)

let append_import_to_body (cb: concrete_module_body) (import: concrete_import_list): concrete_module_body =
  let (ConcreteModuleBody (mn, kind, docstring, imports, decls)) = cb in
  if equal_module_name mn pervasive_module_name then
    cb
  else
    ConcreteModuleBody (mn, kind, docstring, import :: imports, decls)

(** The environment, and the C generated so far, one unit per module in the
    order the modules were compiled. *)
type compiler = Compiler of env * c_unit list

type module_source =
  | TwoFileModuleSource of {
      int_filename: string;
      int_code: string;
      body_filename: string;
      body_code: string
    }
  | BodyModuleSource of {
      body_filename: string;
      body_code: string
    }

let ( let* ) = Outcome.bind

(* An error does not say which module it is in (issue #107). The driver knows,
   and puts the name in the errors of everything that runs once the name is
   known. *)
let in_module (name: module_name) (outcome: 'a Outcome.t): 'a Outcome.t =
  Result.map_error (List.map (fun error -> add_module_name error name)) outcome

let parse_and_combine (env: env) (source: module_source): (module_name * Combined.combined_module) Outcome.t =
  match source with
  | TwoFileModuleSource { int_filename; int_code; body_filename; body_code } ->
     let* (ci: concrete_module_interface) = parse_module_int int_code int_filename in
     let name: module_name = mod_int_name ci in
     in_module name
       (let* (cb: concrete_module_body) = parse_module_body body_code body_filename in
        let ci: concrete_module_interface = append_import_to_interface ci pervasive_imports
        and cb: concrete_module_body = append_import_to_body cb pervasive_imports in
        let* (combined: Combined.combined_module) = combine env ci cb in
        Ok (name, combined))
  | BodyModuleSource { body_filename; body_code } ->
     let* (cb: concrete_module_body) = parse_module_body body_code body_filename in
     let name: module_name = mod_body_name cb in
     in_module name
       (let cb: concrete_module_body = append_import_to_body cb pervasive_imports in
        let* (combined: Combined.combined_module) = body_as_combined env cb in
        Ok (name, combined))

let compile_mod (c: compiler) (source: module_source): compiler Outcome.t =
  let (Compiler (env, units)) = c in
  let* (name, combined) = parse_and_combine env source in
  in_module name
    (let* () = check_ends_in_return combined in
     let* (combined: SmallCombined.combined_module) = DesugaringPass.desugar combined in
     let* ((env, linked): env * linked_module) = extract env combined in
     let* (typed: typed_module) = augment_module env linked in
     let* () = check_module_linearity typed in
     let* (env: env) = extract_bodies env typed in
     let* ((env, mono): env * mono_module) = monomorphize env typed in
     let* (unit: c_unit) = gen_module env mono in
     Ok (Compiler (env, units @ [unit])))

let rec compile_multiple (c: compiler) (modules: module_source list): compiler Outcome.t =
  match modules with
  | m::rest ->
     let* (c: compiler) = compile_mod c m in
     compile_multiple c rest
  | [] ->
     Ok c

let fake_mod_source (is: string) (bs: string): module_source =
  TwoFileModuleSource { int_filename = ""; int_code = is; body_filename = ""; body_code = bs }

let finish (compiler: compiler) (entrypoint: (module_name * identifier) option): c_program Outcome.t =
  let (Compiler (env, units)) = compiler in
  (* The wrapper functions. *)
  let* (env, decls) = monomorphize_wrappers env in
  let* wrapper_unit = gen_module env (MonoModule (make_mod_name BuiltInNames.wrappers_module, decls)) in
  let* wrappers = Outcome.map (fun decls -> CUnit (BuiltInNames.wrappers_unit_name, decls)) (all_wrappers env) in
  let* entrypoint =
    match entrypoint with
    | Some (mn, i) ->
       Outcome.map Option.some (entrypoint_code env (make_qident (mn, i, i)))
    | None ->
       Ok None
  in
  Ok (CProgram { prelude = CSource prelude_c; units = units @ [wrapper_unit; wrappers]; entrypoint })

let empty_compiler (): compiler Outcome.t =
  (* We have to compile the Austral.Pervasive module, followed by
     Austral.Memory, since the latter uses declarations from the former. *)
  let c = Compiler (empty_env, []) in
  let* (c: compiler) = compile_mod c (fake_mod_source pervasive_interface_source pervasive_body_source) in
  compile_mod c (fake_mod_source memory_interface_source memory_body_source)

(* The built-in modules, then the given ones, in order. *)
let compile_modules (sources: module_source list): compiler Outcome.t =
  let* (c: compiler) = empty_compiler () in
  compile_multiple c sources

let compile (sources: module_source list) (entrypoint: (module_name * identifier) option): c_program Outcome.t =
  let* (compiler: compiler) = compile_modules sources in
  finish compiler entrypoint

let check (sources: module_source list): unit Outcome.t =
  Outcome.map ignore (compile_modules sources)
