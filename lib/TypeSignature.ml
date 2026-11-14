(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Type
open TypeParameters

(** Represents the interface to a type: its name, list of type parameters, and
    universe. We use this as part of the semantic extraction process: to
    validate the definition of a type, we need to know about other types defined
    in the module. To break the circularity, we have a pass that extracts type
    signatures first, and then validates type structures against those
    signatures.

    The ID is the one of the record or union the signature belongs to. The
    environment reserves it before the type is registered (see
    {!Env.reserve_record_id}), so that the types of the module can name it. *)
type type_signature = TypeSignature of identifier * typarams * universe * Id.type_decl_id
