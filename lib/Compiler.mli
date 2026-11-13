(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open CRepr

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

(** Compile the built-in modules and then the given modules, in order, and
    return the whole C program: a unit per module, the wrappers of the
    exported functions and, if asked for, the entrypoint. The entrypoint is a
    module name and the name of a function in it.

    This reads no file, writes none, and raises no [Austral_error]: the error
    that stopped it is in the result. *)
val compile : module_source list -> (module_name * identifier) option -> c_program Outcome.t

(** What the type-check mode of the command line runs: every stage of
    {!compile} on every module, with the C thrown away. The step after the
    modules does not run, so an error that only the wrappers or the entrypoint
    would find is not reported. *)
val check : module_source list -> unit Outcome.t
