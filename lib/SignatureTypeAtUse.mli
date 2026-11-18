(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Type

(** A type of the signature of a declaration, as a use of the declaration has
    it: the type of a parameter, a slot or a return value, the argument of an
    instance, or a record or union applied to its own type parameters. Every
    type variable of the type is {!Type.BoundAtUse}.

    The declaration holds the type with its type parameters standing for
    themselves, which is what they do in its body. A use matches the result
    against the types it has ({!TypeMatch.match_type}), and the match binds the
    type parameters. Without this a function that calls itself could not tell
    its own type parameter from that of the function it calls (issue #156). *)
val signature_type_at_use : ty -> ty
