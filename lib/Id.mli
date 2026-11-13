(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** This module contains opaque types for IDs used by the environment.

The reason this is defined outside the `Env` module is to avoid a cyclic
dependency (`Env` depends on `Tast` and `Tast` depends on IDs).

An ID is a number that nothing outside this module can see or choose. The only
way to get an ID is to take the next one from a supply. The environment has
the supply, and makes an ID when something is registered in it. *)

(** The type of module IDs. *)
type mod_id
[@@deriving eq]

(** The type of declaration IDs. *)
type decl_id
[@@deriving (eq, show)]

(** The type of instance method IDs. *)
type ins_meth_id
[@@deriving (eq, show)]

(** The type of monomorph IDs. *)
type mono_id
[@@deriving (eq, show)]

(** The next ID of each kind, and the supply to take the one after it from. *)

val fresh_mod_id : Supply.t -> mod_id * Supply.t
val fresh_decl_id : Supply.t -> decl_id * Supply.t
val fresh_ins_meth_id : Supply.t -> ins_meth_id * Supply.t
val fresh_mono_id : Supply.t -> mono_id * Supply.t

(** The order IDs were made in. A set of IDs keeps its elements in it. *)

val compare_mod_id : mod_id -> mod_id -> int
val compare_decl_id : decl_id -> decl_id -> int

(** The number of an ID. It is for the name that the generated C gives to what
    the ID identifies, and for nothing else. *)

val decl_id_number : decl_id -> int
val ins_meth_id_number : ins_meth_id -> int
val mono_id_number : mono_id -> int
