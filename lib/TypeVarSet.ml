(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

module TypeVarSet =
  Set.Make(
      struct
        open Type
        type t = type_var
        let compare = compare
      end
    )
