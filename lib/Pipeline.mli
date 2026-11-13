(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** The type of every stage of a compilation, and of the whole.

    Each stage module gives its entry point one of these types, so the type of
    a stage is written here. The two checks are the exception: see
    {!check_returns}. {!Compiler} runs the stages in
    the order they are listed, once per module, and {!finish} once after all
    modules.

    A stage that returns [Outcome.t] can fail. A stage that returns an [env]
    changes the environment. A stage that takes a [Supply.t] makes
    up names or regions: the driver takes the supply out of the environment
    with [Env.with_supply], and the stage returns the supply without the
    numbers it took. *)

open Identifier
open Env
open Cst
open Stages
open CRepr

(** {1 The whole} *)

(** The text of a module: an interface and a body, or a body alone. The file
    names are for the positions in errors. *)
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

(** A module name, and the name of a function in it. *)
type entrypoint = module_name * identifier

(** The built-in modules, then the given ones in order, then {!finish}. *)
type compile = module_source list -> entrypoint option -> c_program Outcome.t

(** The stages of {!compile} on every module, without {!finish}. *)
type check = module_source list -> unit Outcome.t

(** {1 The stages, in order} *)

(** The text of a module interface or body, and the name of its file. *)
type 'tree parse = string -> string -> 'tree Outcome.t

(** An interface and a body make one module. Resolves the imports and
    qualifies the names. *)
type combine = env -> concrete_module_interface -> concrete_module_body -> Combined.combined_module Outcome.t

(** {!combine} for a module that has only a body: every declaration is
    public. *)
type combine_body = env -> concrete_module_body -> Combined.combined_module Outcome.t

(** The return check. {!ReturnCheck.returning_module} is abstract, and only
    this check makes one. [ReturnCheck] cannot name this type, because this
    file names its type; {!Compiler} gives the check this type. *)
type check_returns = Combined.combined_module -> ReturnCheck.returning_module Outcome.t

type desugar = Supply.t -> ReturnCheck.returning_module -> (SmallCombined.combined_module * Supply.t) Outcome.t

type extract = env -> SmallCombined.combined_module -> (env * Linked.linked_module) Outcome.t

type check_types = env -> Supply.t -> Linked.linked_module -> (Tast.typed_module * Supply.t) Outcome.t

(** The linearity check. As with {!check_returns}, only this check makes a
    {!LinearityCheck.checked_module}, and {!Compiler} gives it this type. *)
type check_linearity = Tast.typed_module -> LinearityCheck.checked_module Outcome.t

type extract_bodies = env -> LinearityCheck.checked_module -> env Outcome.t

type monomorphize = env -> LinearityCheck.checked_module -> (env * Mtast.mono_module) Outcome.t

type generate = env -> Supply.t -> Mtast.mono_module -> (c_unit * Supply.t) Outcome.t

(** Once, after all modules: the wrappers of the exported functions and the
    entrypoint, after the units of the modules. *)
type finish = env -> c_unit list -> entrypoint option -> c_program Outcome.t
