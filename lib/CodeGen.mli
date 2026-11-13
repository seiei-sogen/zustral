(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Id
open CRepr

val gen_function_id : function_id -> CIdent.t

val gen_mono_id : mono_id -> CIdent.t

(** The C name of a variable or a parameter. *)
val gen_ident : identifier -> CIdent.t

(** The C type of a type that crosses the C boundary. *)
val gen_foreign_type : MonoType.mono_ty ForeignType.foreign_ty -> c_ty

(** Generate the C for a module.

    This should not be able to fail, and today it can: it finds states the
    earlier stages should have ruled out. When the types rule them out (issue
    #117), this returns a plain [c_unit].

    The variables it makes up are numbered from the supply, which comes back
    without them. *)
val gen_module : Pipeline.generate
