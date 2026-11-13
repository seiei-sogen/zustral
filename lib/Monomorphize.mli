(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Id
open Env
open Stages.Mtast

(** This is the entrypoint to the monomorphization pass.

    Monomorphizing a module involves:

    1. Take all the declarations and monomorphize the ones you can (e.g.:
   concrete function definitions, concrete records, concrete instances).

    2. In the process, monomorphs will be collected into the environment.

    3. Get all uninstantiated monomorphs from the environment, and instantiate
   them. Again, new (uninstantiated) monomorphs will be collected and added to
   the environment: for example, if you instantiate a generic function `f` and
   the body of that function contains a call to a generic function `g` with
   monomorphic arguments, that monomorph of `g` will be added to the
   environment.

    4. Repeat step #3 until there are no more uninstantiated monomorphs in the
   environment.

    5. You're done.

*)
val monomorphize : Pipeline.monomorphize

(** The monomorph of a record or a union for the given type arguments, as every
    use of a type asks for it: registered the first time it is asked for. *)
val monomorphize_named_type : env -> type_decl_id -> MonoType.mono_ty list -> (mono_id * env)

(** Instantiate the monomorphs that are registered and not instantiated, and
    those they register, until none is left. Returns their declarations. *)
val instantiate_monomorphs_until_exhausted : env -> (env * mdecl list)

(** Instantiate the functions the program exports, and everything they use.
    Called once, after all modules. Returns the declarations, and for each
    exported function, in the order the environment lists them, its export
    name, the ID of the monomorph that its wrapper calls and its checked
    signature. *)
val monomorphize_wrappers : env -> (env * mdecl list * (string * mono_id * MonoType.mono_ty ForeignType.foreign_signature) list) Outcome.t
