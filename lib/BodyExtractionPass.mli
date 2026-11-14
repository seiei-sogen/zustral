(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** Given a typed module, extract the function and instance method bodies and
    store them in the environment. It takes a module that passed the linearity
    check, so that the driver can neither skip that check nor run it after
    this stage.

    The only failures are internal errors of the environment: an ID it does
    not know, or a body stored twice. When the environment cannot report those
    (issue #114), this returns a plain [env]. *)
val extract_bodies : Pipeline.extract_bodies
