(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Type
open TypeParameter
open EnvTypes

(** Where an instance is declared, check its methods against the methods of its
    typeclass (#91): the instance defines every method of the typeclass exactly
    once, and each with the type parameters, the parameter types and the return
    type of the typeclass's method, with the instance's argument in place of
    the typeclass's parameter.

    The arguments are the place of the instance, the name of the typeclass, its
    parameter, the instance's argument, the typeclass's methods and the
    instance's methods. *)
val check_instance_methods_match_typeclass : Error.place -> identifier -> type_parameter -> ty -> typeclass_method_decl list -> instance_method_input list -> unit
