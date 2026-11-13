(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Region

type universe =
  | FreeUniverse
  | LinearUniverse
  | TypeUniverse
  | RegionUniverse
[@@deriving (eq, show)]

type integer_width =
  | Width8
  | Width16
  | Width32
  | Width64
  | WidthByteSize
  | WidthIndex
[@@deriving (eq, show)]

type signedness =
  | Unsigned
  | Signed
[@@deriving (eq, show)]

type type_var = TypeVariable of identifier * universe * qident * sident list
[@@deriving (eq, show)]

type ty =
  | Unit
  | Boolean
  | Integer of signedness * integer_width
  | SingleFloat
  | DoubleFloat
  | NamedType of qident * ty list * universe
  | RegionTy of region
  | ReadRef of ty * ref_region
  | WriteRef of ty * ref_region
  | Span of ty * ref_region
  | SpanMut of ty * ref_region
  | TyVar of type_var
  | Address of ty
  | Pointer of ty
  | FnPtr of ty list * ty
  (** Special case, see the `mono_to_ty` function. We need this to be able to do
     monomorph instantiation, but this doesn't correspond to anything in the
     code. *)
(** The region of a reference or span type. *)
and ref_region =
  | RefRegion of region
  | RefRegionVar of type_var
  (** A type variable of the Region universe. *)
  | NotARegion of ty
  (** Whatever else the program wrote in the region position: the type parser
      does not check it, so [&\[Int32, Int32\]] is accepted (#90). *)
[@@deriving show]

type typed_slot = TypedSlot of identifier * ty

type typed_case = TypedCase of identifier * typed_slot list

type value_parameter = ValueParameter of identifier * ty
[@@deriving show]

(** The region field as a type. *)
val ty_of_ref_region : ref_region -> ty

(** A type in the region position: a region, a region variable, or anything
    else as it was written (#90). *)
val ref_region_of_ty : ty -> ref_region

val universe_string : universe -> string

(* A string representation of a type, for showing to the user. *)
val type_string : ty -> string

(* The type of array sizes and indices *)
val index_type : ty

val string_type : ty

val equal_ty : ty -> ty -> bool
