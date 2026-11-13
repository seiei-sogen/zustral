(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** What a stage of the compiler returns: its result, or the errors that
    stopped it. Inside a stage an error travels as the exception
    [Austral_error]; the functions the driver calls turn it into a value with
    {!catch}, so that no exception leaves a stage
    ([tools/refactoring/escape-check.py] checks this). *)
open Error

type 'a t = ('a, austral_error list) result

(** Run the callback, and return the error it raises as a value. Other
    exceptions pass through. *)
val catch : (unit -> 'a) -> 'a t

(** The result, or the first error raised again. For the driver, until it
    chains the stages by their values. *)
val or_raise : 'a t -> 'a
