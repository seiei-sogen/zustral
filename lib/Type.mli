(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
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

(** What the matching of two types does with a type variable ({!TypeMatch}).
    Either way the variable is a type parameter of a declaration. *)
type type_var_matching =
  | StandsForItself
  (** In the types of a declaration as it is declared, its signature and its
      body: the type parameter is the same type as itself and as no other. *)
  | BoundAtUse
  (** In the types of the signature of a declaration where it is used: a
      function or a method that is called, a record or a union that is
      constructed or taken apart, an instance that is looked up. Matching
      binds the type parameter to the type the use has in its place. *)
[@@deriving eq]

(** A type parameter in a type: its name, its universe, the name of the
    declaration it is a parameter of, its constraints, and what matching does
    with it. *)
type type_var = TypeVariable of identifier * universe * qident * sident list * type_var_matching
[@@deriving eq]

type ty =
  | Unit
  | Boolean
  | Integer of signedness * integer_width
  | SingleFloat
  | DoubleFloat
  | NamedType of qident * Id.type_decl_id * ty list * universe
  (** The name, as the program wrote it, and the ID of the record or union
      it stands for. *)
  | RegionTy of region
  | ReadRef of ty * ref_region
  | WriteRef of ty * ref_region
  | Span of ty * ref_region
  | SpanMut of ty * ref_region
  | TyVar of type_var
  | Address of ty
  | Pointer of ty
  | FnPtr of ty list * ty
(** The region of a reference or span type. *)
and ref_region =
  | RefRegion of region
  | RefRegionVar of type_var
  (** A type variable of the Region universe. *)

type typed_slot = TypedSlot of identifier * ty

type typed_case = TypedCase of identifier * typed_slot list

type value_parameter = ValueParameter of identifier * ty

(** The region field as a type. *)
val ty_of_ref_region : ref_region -> ty

(** A region or a type variable of the Region universe as the region of a
    reference or a span; [None] for any other type. *)
val ref_region_of_ty : ty -> ref_region option

val universe_string : universe -> string

(* A string representation of a type, for showing to the user. *)
val type_string : ty -> string

(* The type of array sizes and indices *)
val index_type : ty

val string_type : ty

val equal_ty : ty -> ty -> bool
