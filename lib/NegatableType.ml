(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Type

type t =
  | NegatableInt8
  | NegatableInt16
  | NegatableInt32
  | NegatableInt64
  | NegatableFloat32
  | NegatableFloat64

let negatable_types: (ty * t) list = [
    (Integer (Signed, Width8), NegatableInt8);
    (Integer (Signed, Width16), NegatableInt16);
    (Integer (Signed, Width32), NegatableInt32);
    (Integer (Signed, Width64), NegatableInt64);
    (SingleFloat, NegatableFloat32);
    (DoubleFloat, NegatableFloat64);
  ]

let of_type (ty: ty): t option =
  Option.map snd (List.find_opt (fun (negatable, _) -> equal_ty negatable ty) negatable_types)
