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
type constant_id = decl_id

type union_case_id = decl_id

type typeclass_method_id = decl_id

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

type type_decl_id =
  | RecordTypeId of record_id
  | UnionTypeId of union_id

type ins_meth_id = InsMethId of int
[@@deriving (eq, show)]

type mono_id = MonoId of int
[@@deriving (eq, show)]

(* As with the kinds of declaration ID: one sequence, types that differ only
   outside this module. *)
type record_mono_id = mono_id
[@@deriving eq]

type union_mono_id = mono_id
[@@deriving eq]

type function_mono_id = mono_id
[@@deriving eq]

type method_mono_id = mono_id
[@@deriving eq]

(* ID utilities *)

let fresh_mod_id (supply: Supply.t): mod_id * Supply.t =
  let (number, supply) = Supply.next_mod_id supply in
  (ModId number, supply)

let fresh_decl_id (supply: Supply.t): decl_id * Supply.t =
  let (number, supply) = Supply.next_decl_id supply in
  (DeclId number, supply)

let fresh_constant_id: Supply.t -> constant_id * Supply.t =
  fresh_decl_id

let fresh_union_case_id: Supply.t -> union_case_id * Supply.t =
  fresh_decl_id

let fresh_typeclass_method_id: Supply.t -> typeclass_method_id * Supply.t =
  fresh_decl_id

let fresh_function_id: Supply.t -> function_id * Supply.t =
  fresh_decl_id

let fresh_typeclass_id: Supply.t -> typeclass_id * Supply.t =
  fresh_decl_id

let fresh_type_id (supply: Supply.t): decl_id * Supply.t =
  let (number, supply) = Supply.next_type_id supply in
  (DeclId number, supply)

let fresh_record_id: Supply.t -> record_id * Supply.t =
  fresh_type_id

let fresh_union_id: Supply.t -> union_id * Supply.t =
  fresh_type_id

let fresh_instance_id: Supply.t -> instance_id * Supply.t =
  fresh_decl_id

let fresh_ins_meth_id (supply: Supply.t): ins_meth_id * Supply.t =
  let (number, supply) = Supply.next_ins_meth_id supply in
  (InsMethId number, supply)

let fresh_mono_id (supply: Supply.t): mono_id * Supply.t =
  let (number, supply) = Supply.next_mono_id supply in
  (MonoId number, supply)

let fresh_record_mono_id: Supply.t -> record_mono_id * Supply.t =
  fresh_mono_id

let fresh_union_mono_id: Supply.t -> union_mono_id * Supply.t =
  fresh_mono_id

let fresh_function_mono_id: Supply.t -> function_mono_id * Supply.t =
  fresh_mono_id

let fresh_method_mono_id: Supply.t -> method_mono_id * Supply.t =
  fresh_mono_id

let mono_id_of_record (id: record_mono_id): mono_id =
  id

let mono_id_of_union (id: union_mono_id): mono_id =
  id

let mono_id_of_function (id: function_mono_id): mono_id =
  id

let mono_id_of_method (id: method_mono_id): mono_id =
  id

let compare_instance_id (DeclId a: instance_id) (DeclId b: instance_id): int =
  compare a b

let function_id_number (DeclId number: function_id): int =
  number

let ins_meth_id_number (InsMethId number): int =
  number

let mono_id_number (MonoId number): int =
  number
