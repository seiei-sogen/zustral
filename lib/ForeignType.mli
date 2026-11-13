(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The types that may cross the C boundary, in the signature of a function
    imported with [Foreign_Import] or exported with [Foreign_Export]. *)
open Identifier
open Type
open Error

(** Which way a function crosses the C boundary. *)
type direction =
  | Import
  (** [Foreign_Import]: Austral calls a C function. *)
  | Export
  (** [Foreign_Export]: C calls an Austral function. *)

(** A type that may cross the C boundary, by what C sees. ['leaf] is the
    Austral type a pointer points to: {!Type.ty} when the signature is checked,
    {!MonoType.mono_ty} after monomorphization. *)
type 'leaf foreign_ty =
  | ForeignBoolean
  | ForeignInteger of signedness * integer_width
  | ForeignSingleFloat
  | ForeignDoubleFloat
  | ForeignFnPtr
  | ForeignAddress of 'leaf foreign_ty
  (** An exported [Address], whose pointee may cross the boundary too. *)
  | ForeignPointer of 'leaf
  (** An imported [Address] or [Pointer], to anything. *)
  | ForeignSpan of 'leaf
  (** An imported span, of anything: C receives a pointer to its elements. *)

(** The signature of a function that crosses the C boundary. *)
type 'leaf foreign_signature = {
    params: (identifier * 'leaf foreign_ty) list;
    rt: 'leaf foreign_ty;
  }

(** Check that the parameters and the return type of a non-generic function
    may cross the C boundary in the given direction. Raises at the given place:
    [Not allowed] for an import, {!TypeErrors.ExportDisallowedType} for an
    export. *)
val check_signature : place -> direction -> value_parameter list -> ty -> ty foreign_signature
