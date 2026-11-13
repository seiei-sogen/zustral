(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)


type region
[@@deriving (eq, show)]

val region_id: region -> int

val static_region : region

(** A region no other borrow has, and the supply to take the next one from. *)
val fresh_region : Supply.t -> region * Supply.t
