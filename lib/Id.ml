(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

type mod_id = ModId of int
[@@deriving eq]

type decl_id = DeclId of int
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

let fresh_ins_meth_id (supply: Supply.t): ins_meth_id * Supply.t =
  let (number, supply) = Supply.next_ins_meth_id supply in
  (InsMethId number, supply)

let fresh_mono_id (supply: Supply.t): mono_id * Supply.t =
  let (number, supply) = Supply.next_mono_id supply in
  (MonoId number, supply)

let compare_mod_id (ModId a) (ModId b): int =
  compare a b

let compare_decl_id (DeclId a) (DeclId b): int =
  compare a b

let decl_id_number (DeclId number): int =
  number

let ins_meth_id_number (InsMethId number): int =
  number

let mono_id_number (MonoId number): int =
  number
