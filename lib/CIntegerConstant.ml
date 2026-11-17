(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

let text_of_value (value: Z.t): string =
  let two_to_the_63: Z.t = Z.shift_left Z.one 63 in
  if Z.equal value (Z.neg two_to_the_63) then
    "(-9223372036854775807 - 1)"
  else if Z.geq value two_to_the_63 then
    Z.to_string value ^ "U"
  else
    Z.to_string value
