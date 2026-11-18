(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
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
  (** [Foreign_Import]: Zustral calls a C function. *)
  | Export
  (** [Foreign_Export]: C calls a Zustral function. *)

(** A type that may cross the C boundary, by what C sees. ['leaf] is the
    Zustral type a pointer points to: {!Type.ty} when the signature is checked,
    {!MonoType.mono_ty} after monomorphization. The type is private: a value
    comes from {!check_signature}, or from {!map_signature} on one that did,
    so a signature that is not allowed cannot be built. *)
type 'leaf foreign_ty = private
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

(** What a function that crosses the C boundary returns, by what C sees. *)
type 'leaf foreign_return = private
  | ForeignReturnsNothing
  (** An imported function whose return type is [Unit]: the C function returns
      [void]. *)
  | ForeignReturns of 'leaf foreign_ty

(** The signature of a function that crosses the C boundary. *)
type 'leaf foreign_signature = private {
    params: (identifier * 'leaf foreign_ty) list;
    rt: 'leaf foreign_return;
  }

(** Check that the parameters and the return type of the named non-generic
    function may cross the C boundary in the given direction. Raises at the
    given place: {!TypeErrors.ImportDisallowedType} for an import,
    {!TypeErrors.ExportDisallowedType} for an export. *)
val check_signature : place -> direction -> identifier -> value_parameter list -> ty -> ty foreign_signature

(** Replace each pointee of a signature, the return type's first, then the
    parameters' in order. [f] gets the state and the pointee, and returns the
    new pointee with the state for the next one. *)
val map_signature : ('state -> 'leaf -> 'mapped * 'state) -> 'state -> 'leaf foreign_signature -> 'mapped foreign_signature * 'state
