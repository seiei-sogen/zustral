(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open TypeBindings
open Stages.Tast

(** Replace the type variables in a typed statement. *)
val replace_tyvars_stmt : type_bindings -> tstmt -> tstmt
