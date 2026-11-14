(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** Rewrite the statements and expressions of a module into the smaller
    language the later stages take. It fails on a borrow expression in the
    value of a constant: there is no statement to put the borrow in.
    It takes a module that passed the return check, so that the driver can
    neither skip that check nor run it after this stage.

    The names it makes up come from the supply, and it returns the supply
    without them. *)
val desugar : Pipeline.desugar
