(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier

(* The number tells regions apart. The name is what an error message shows. *)
type region = Region of int * identifier
[@@deriving (eq, show)]

let region_name (Region (_, name)) = name

let static_region = Region (0, make_ident (BuiltInNames.type_name BuiltInNames.Static))

let fresh_region (name: identifier) (supply: Supply.t): region * Supply.t =
  let (i, supply) = Supply.next_region supply in
  (Region (i, name), supply)