(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Cst

(* Zustral.Pervasive *)

val pervasive_module_name : module_name

val pervasive_imports: concrete_import_list

(* Zustral.Memory *)

val memory_module_name : module_name

val is_address_type : qident -> bool
val is_pointer_type : qident -> bool
