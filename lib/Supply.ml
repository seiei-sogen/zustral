(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

type t = {
    mod_ids: int;
    decl_ids: int;
    ins_meth_ids: int;
    mono_ids: int;
    temporaries: int;
  }

(* Where a kind starts is visible in the generated C and in messages, like the
   numbers themselves. *)
let initial: t = {
    mod_ids = 1;
    decl_ids = 1;
    ins_meth_ids = 1;
    mono_ids = 1;
    temporaries = 0;
  }

let next_mod_id (supply: t): int * t =
  (supply.mod_ids, { supply with mod_ids = supply.mod_ids + 1 })

let next_decl_id (supply: t): int * t =
  (supply.decl_ids, { supply with decl_ids = supply.decl_ids + 1 })

let next_ins_meth_id (supply: t): int * t =
  (supply.ins_meth_ids, { supply with ins_meth_ids = supply.ins_meth_ids + 1 })

let next_mono_id (supply: t): int * t =
  (supply.mono_ids, { supply with mono_ids = supply.mono_ids + 1 })

let next_temporary (supply: t): int * t =
  (supply.temporaries, { supply with temporaries = supply.temporaries + 1 })

let rec map (f: t -> 'a -> 'b * t) (supply: t) (list: 'a list): 'b list * t =
  match list with
  | first :: rest ->
     let (first, supply) = f supply first in
     let (rest, supply) = map f supply rest in
     (first :: rest, supply)
  | [] ->
     ([], supply)
