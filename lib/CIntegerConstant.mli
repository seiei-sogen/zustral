(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The integer constants of the generated C. *)

(** The C text of an integer value, in decimal. A decimal constant of C has the
    first signed type that holds it, and none of 64 bits holds a value from
    2^63 up: such a value gets the suffix [U]. The least [Int64], -2^63, is
    the negation of such a constant, so it is written as
    [(-9223372036854775807 - 1)]. *)
val text_of_value : Z.t -> string
