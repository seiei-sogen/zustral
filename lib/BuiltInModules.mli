(*
   Part of the Zustral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Sources of the builtin modules and the prelude, injected into OCaml source
    by the concat_builtins.py script. *)

(** Source code of the Zustral.Pervasive interface file. *)
val pervasive_interface_source: string
(** Source code of the Zustral.Pervasive body file. *)
val pervasive_body_source: string

(** Source code of the Zustral.Memory interface file. *)
val memory_interface_source: string
(** Source code of the Zustral.Memory body file. *)
val memory_body_source: string

(** Source code of prelude.c *)
val prelude_c: string
