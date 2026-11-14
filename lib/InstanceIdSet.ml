(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

module InstanceIdSet =
  Set.Make(
      struct
        open Id
        type t = instance_id
        let compare: t -> t -> int = compare_instance_id
      end
    )
