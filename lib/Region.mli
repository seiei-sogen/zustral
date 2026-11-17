(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)


type region
[@@deriving (eq, show)]

(** The name of the region: `Static`, or the name of the borrow that made it. *)
val region_name : region -> Identifier.identifier

val static_region : region

(** A region no other borrow has, with the borrow's name for its region, and the
    supply to take the next one from. *)
val fresh_region : Identifier.identifier -> Supply.t -> region * Supply.t
