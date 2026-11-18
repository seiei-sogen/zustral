(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** The types whose values the arithmetic negation `-x` applies to: the signed
    integer types and the float types. Negating a value of an unsigned type
    fails for every value but zero, so no such type is here (issue #26). *)

type t =
  | NegatableInt8
  | NegatableInt16
  | NegatableInt32
  | NegatableInt64
  | NegatableFloat32
  | NegatableFloat64

(** The negatable type that the type is, if it is one. A type parameter is
    none: the type checker accepts its value when the parameter has the
    constraint [TrappingArithmetic], and an instance of the generic code has
    to give it a negatable type. *)
val of_type : Type.ty -> t option
