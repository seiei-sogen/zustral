(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The type checker. *)

(** The context for statement checking. *)
type stmt_ctx

(** Check the types of a module. The regions of its borrows are numbered from
    the supply, which comes back without them. *)
val augment_module : Pipeline.check_types
