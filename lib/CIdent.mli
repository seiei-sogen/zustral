(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The names in the generated C. A name is made from where it comes from, and
    [render] alone decides how it is spelled in C. *)

(** A C identifier. *)
type t

(** A name the compiler builds (a declaration id, a monomorph id, a temporary)
    or one the prelude defines. It is spelled as it is: no such name is a C
    reserved word. *)
val generated : string -> t

(** A name the user wrote in Zustral: a variable, a parameter, a record slot, a
    union case, a constant. It is spelled with the reserved-word escape. *)
val zustral : string -> t

(** A name the user gave in a pragma for a C symbol. It is spelled as it is,
    so the pragma must give a C identifier that is not a C reserved word. *)
val external_name : string -> t

(** Whether a name is a C reserved word. *)
val is_c_reserved_word : string -> bool

(** Whether two names are spelled the same in C. *)
val equal : t -> t -> bool

(** The spelling in C. The reserved-word escape puts [au_ckw_] before a name
    that is a C reserved word: before [double], and not before [doubled]. *)
val render : t -> string
