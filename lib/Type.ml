(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(* A catch-all arm over a variant type is an error in this file: a new
   constructor must stop the build at every match. *)
[@@@warning "@4"]

open Id
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
  | MonoTy of mono_id
(* The region of a reference or span type. *)
and ref_region =
  | RefRegion of region
  | RefRegionVar of type_var
  (* A type variable of the Region universe. *)
  | NotARegion of ty
  (* Whatever else the program wrote in the region position: the type parser
     does not check it, so `&[Int32, Int32]` is accepted (#90). *)
[@@deriving show]

type typed_slot = TypedSlot of identifier * ty

type typed_case = TypedCase of identifier * typed_slot list

type value_parameter = ValueParameter of identifier * ty
[@@deriving show]

(* Matching, equality, substitution and printing see the region field as a
   type, so that a NotARegion behaves as the type it holds (#90). *)
let ty_of_ref_region (r: ref_region): ty =
  match r with
  | RefRegion r -> RegionTy r
  | RefRegionVar tv -> TyVar tv
  | NotARegion ty -> ty

let ref_region_of_ty (ty: ty): ref_region =
  match ty with
  | RegionTy r ->
     RefRegion r
  | TyVar (TypeVariable (_, universe, _, _) as tv) ->
     (match universe with
      | RegionUniverse ->
         RefRegionVar tv
      | FreeUniverse | LinearUniverse | TypeUniverse ->
         NotARegion ty)
  | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | ReadRef _
  | WriteRef _ | Span _ | SpanMut _ | Address _ | Pointer _ | FnPtr _ | MonoTy _ ->
     NotARegion ty

let universe_string = function
  | FreeUniverse -> "Free"
  | LinearUniverse -> "Linear"
  | TypeUniverse -> "Type"
  | RegionUniverse -> "Region"

(* ByteSize and Index have one name each, whatever the signedness. *)
let integer_built_in (s: signedness) (w: integer_width): BuiltInNames.builtin_type =
  match (s, w) with
  | (Unsigned, Width8) -> BuiltInNames.Nat8
  | (Unsigned, Width16) -> BuiltInNames.Nat16
  | (Unsigned, Width32) -> BuiltInNames.Nat32
  | (Unsigned, Width64) -> BuiltInNames.Nat64
  | (Signed, Width8) -> BuiltInNames.Int8
  | (Signed, Width16) -> BuiltInNames.Int16
  | (Signed, Width32) -> BuiltInNames.Int32
  | (Signed, Width64) -> BuiltInNames.Int64
  | (Unsigned, WidthByteSize) | (Signed, WidthByteSize) -> BuiltInNames.ByteSize
  | (Unsigned, WidthIndex) | (Signed, WidthIndex) -> BuiltInNames.Index

let rec type_string = function
  | Unit ->
     BuiltInNames.type_name BuiltInNames.Unit
  | Boolean ->
     BuiltInNames.type_name BuiltInNames.Bool
  | Integer (s, w) ->
     BuiltInNames.type_name (integer_built_in s w)
  | SingleFloat ->
     BuiltInNames.type_name BuiltInNames.Float32
  | DoubleFloat ->
     BuiltInNames.type_name BuiltInNames.Float64
  | NamedType (n, args, _) ->
     (ident_string (local_name n)) ^ args_string args
  | RegionTy r ->
     "Region<" ^ (string_of_int (region_id r)) ^ ">"
  | ReadRef (t, r) ->
     BuiltInNames.read_ref_name ^ "[" ^ (type_string t) ^ ", " ^ (type_string (ty_of_ref_region r)) ^ "]"
  | WriteRef (t, r) ->
     BuiltInNames.write_ref_name ^ "[" ^ (type_string t) ^ ", " ^ (type_string (ty_of_ref_region r)) ^ "]"
  | Span (t, r) ->
     BuiltInNames.span_name ^ "[" ^ (type_string t) ^ ", " ^ (type_string (ty_of_ref_region r)) ^ "]"
  | SpanMut (t, r) ->
     BuiltInNames.span_mut_name ^ "[" ^ (type_string t) ^ ", " ^ (type_string (ty_of_ref_region r)) ^ "]"
  | TyVar (TypeVariable (n, _, _, _)) ->
     ident_string n
  | Address ty ->
     BuiltInNames.address_name ^ "[" ^ (type_string ty) ^ "]"
  | Pointer ty ->
     BuiltInNames.pointer_name ^ "[" ^ (type_string ty) ^ "]"
  | FnPtr (args, rt) ->
     (match args with
      | [] ->
         (* No arguments case. *)
         (BuiltInNames.type_name BuiltInNames.Fn ^ "[" ^ (type_string rt) ^ "]")
      | _ ->
         (* Some arguments case. *)
         let args': string = String.concat ", " (List.map type_string args) in
         (BuiltInNames.type_name BuiltInNames.Fn ^ "[" ^ args' ^ ", " ^ (type_string rt) ^ "]"))
  | MonoTy _ ->
     "MonoTy)"

and args_string = function
  | (first::rest) -> "[" ^ (String.concat ", " (List.map type_string (first::rest))) ^ "]"
  | [] -> ""

let index_type = Integer (Unsigned, WidthIndex)

let string_type = Span (Integer (Unsigned, Width8), RefRegion static_region)

let rec equal_ty a b =
  match a with
  | Unit ->
     (match b with
      | Unit ->
         true
      | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
      | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ ->
         false)
  | Boolean ->
     (match b with
      | Boolean ->
         true
      | Unit | Integer _ | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
      | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ ->
         false)
  | Integer (s, w) ->
     (match b with
      | Integer (s', w') ->
         (equal_signedness s s') && (equal_integer_width w w')
      | Unit | Boolean | SingleFloat | DoubleFloat | NamedType _ | RegionTy _
      | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ ->
         false)
  | SingleFloat ->
     (match b with
      | SingleFloat ->
         true
      | Unit | Boolean | Integer _ | DoubleFloat | NamedType _ | RegionTy _ | ReadRef _
      | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
      | MonoTy _ ->
         false)
  | DoubleFloat ->
     (match b with
      | DoubleFloat ->
         true
      | Unit | Boolean | Integer _ | SingleFloat | NamedType _ | RegionTy _ | ReadRef _
      | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
      | MonoTy _ ->
         false)
  | NamedType (n, args, u) ->
     (match b with
      | NamedType (n', args', u') ->
         (equal_qident n n')
         && (List.for_all (fun (a', b') -> equal_ty a' b') (List.map2 (fun a' b' -> (a',b')) args args'))
         && (equal_universe u u')
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | RegionTy _ | ReadRef _
      | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _ | FnPtr _
      | MonoTy _ ->
         false)
  | RegionTy r ->
     (match b with
      | RegionTy r' ->
         equal_region r r'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ ->
         false)
  | ReadRef (ty, r) ->
     (match b with
      | ReadRef (ty', r') ->
         (equal_ty ty ty') && (equal_ty (ty_of_ref_region r) (ty_of_ref_region r'))
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ ->
         false)
  | WriteRef (ty, r) ->
     (match b with
      | WriteRef (ty', r') ->
         (equal_ty ty ty') && (equal_ty (ty_of_ref_region r) (ty_of_ref_region r'))
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | Span _ | SpanMut _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ ->
         false)
    | Span (ty, r) ->
     (match b with
      | Span (ty', r') ->
         (equal_ty ty ty') && (equal_ty (ty_of_ref_region r) (ty_of_ref_region r'))
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | SpanMut _ | TyVar _ | Address _
      | Pointer _ | FnPtr _ | MonoTy _ ->
         false)
  | SpanMut (ty, r) ->
     (match b with
      | SpanMut (ty', r') ->
         (equal_ty ty ty') && (equal_ty (ty_of_ref_region r) (ty_of_ref_region r'))
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | TyVar _ | Address _ | Pointer _
      | FnPtr _ | MonoTy _ ->
         false)
  | TyVar v ->
     (match b with
      | TyVar v' ->
         equal_type_var v v'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | Address _
      | Pointer _ | FnPtr _ | MonoTy _ ->
         false)
  | Address ty ->
     (match b with
      | Address ty' ->
         equal_ty ty ty'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Pointer _
      | FnPtr _ | MonoTy _ ->
        false)
  | Pointer ty ->
     (match b with
      | Pointer ty' ->
         equal_ty ty ty'
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _
      | FnPtr _ | MonoTy _ ->
         false)
  | FnPtr (args, rt) ->
     (match b with
      | FnPtr (args', rt') ->
         (List.for_all (fun (a', b') -> equal_ty a' b') (List.map2 (fun a' b' -> (a', b')) args args'))
         && (equal_ty rt rt')
      | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
      | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _
      | Pointer _ | MonoTy _ ->
        false)
  | MonoTy a ->
    (match b with
     | MonoTy b ->
       equal_mono_id a b
     | Unit | Boolean | Integer _ | SingleFloat | DoubleFloat | NamedType _
     | RegionTy _ | ReadRef _ | WriteRef _ | Span _ | SpanMut _ | TyVar _ | Address _
     | Pointer _ | FnPtr _ ->
       false)
