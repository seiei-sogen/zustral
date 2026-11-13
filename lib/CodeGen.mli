(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Id
open Type
open Env
open Stages.Mtast
open CRepr

val gen_function_id : function_id -> CIdent.t

val gen_mono_id : mono_id -> CIdent.t

(** The C name of a variable or a parameter. *)
val gen_ident : identifier -> CIdent.t

val fn_type : c_ty

(** The C type of an integer type. *)
val gen_integer_type : signedness -> integer_width -> c_ty

(** Generate the C for a module.

    This should not be able to fail, and today it can: a foreign function
    whose types C cannot take is found here, and so are states the earlier
    stages should have ruled out. When the type checker decides the first
    (issue #115) and the types rule out the second (issue #117), this returns
    a plain [c_unit].

    The variables it makes up are numbered from the supply, which comes back
    without them. *)
val gen_module : env -> Supply.t -> mono_module -> (c_unit * Supply.t) Outcome.t
