(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The check of the [External_Name] of a [Foreign_Import] or a
    [Foreign_Export] pragma. *)

(** Raises a declaration error at the span of the name's string constant unless
    the name is a C identifier that is not a C reserved word. The generated C
    has the name as written (issues #92 and #161). *)
val external_name_must_be_a_c_identifier : Span.span -> string -> unit
