(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

type region = Region of int
[@@deriving (eq, show)]

let region_id (Region i) = i

let static_region = Region (0)

let fresh_region (supply: Supply.t): region * Supply.t =
  let (i, supply) = Supply.next_region supply in
  (Region i, supply)