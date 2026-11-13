(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

type mod_id = ModId of int
[@@deriving eq]

type decl_id = DeclId of int
[@@deriving (eq, show)]

(* Inside this module an ID of one kind is a declaration ID, so the two take
   their numbers from one sequence and print the same. The interface hides
   that: outside, the types differ. *)
type function_id = decl_id
[@@deriving (eq, show)]

type typeclass_id = decl_id
[@@deriving (eq, show)]

type record_id = decl_id
[@@deriving (eq, show)]

type union_id = decl_id
[@@deriving (eq, show)]

type instance_id = decl_id
[@@deriving (eq, show)]

type ins_meth_id = InsMethId of int
[@@deriving (eq, show)]

type mono_id = MonoId of int
[@@deriving (eq, show)]

(* ID utilities *)

let fresh_mod_id (supply: Supply.t): mod_id * Supply.t =
  let (number, supply) = Supply.next_mod_id supply in
  (ModId number, supply)

let fresh_decl_id (supply: Supply.t): decl_id * Supply.t =
  let (number, supply) = Supply.next_decl_id supply in
  (DeclId number, supply)

let fresh_function_id: Supply.t -> function_id * Supply.t =
  fresh_decl_id

let fresh_typeclass_id: Supply.t -> typeclass_id * Supply.t =
  fresh_decl_id

let fresh_record_id: Supply.t -> record_id * Supply.t =
  fresh_decl_id

let fresh_union_id: Supply.t -> union_id * Supply.t =
  fresh_decl_id

let fresh_instance_id: Supply.t -> instance_id * Supply.t =
  fresh_decl_id

let fresh_ins_meth_id (supply: Supply.t): ins_meth_id * Supply.t =
  let (number, supply) = Supply.next_ins_meth_id supply in
  (InsMethId number, supply)

let fresh_mono_id (supply: Supply.t): mono_id * Supply.t =
  let (number, supply) = Supply.next_mono_id supply in
  (MonoId number, supply)

let compare_mod_id (ModId a) (ModId b): int =
  compare a b

let compare_instance_id (DeclId a: instance_id) (DeclId b: instance_id): int =
  compare a b

let function_id_number (DeclId number: function_id): int =
  number

let ins_meth_id_number (InsMethId number): int =
  number

let mono_id_number (MonoId number): int =
  number
