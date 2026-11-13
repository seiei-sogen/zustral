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

(** Give the result to the next step, or stop at the failure. This is how
    stages are chained: a stage needs the value the one before it returned. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** Apply a function that cannot fail to the result. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Apply a function to the elements of a list, in order, and return the
    results. This is how the declarations of a module are gone through. It
    stops at the first failure: the elements after it are not looked at. *)
val traverse : ('a -> 'b t) -> 'a list -> 'b list t

(** {!traverse} for a loop whose steps hand a context on, the way the
    declarations of a module hand on the environment. It cannot go on after a
    failure: the next step needs the context the failed one did not return. *)
val traverse_with_context : ('c * 'a -> ('c * 'b) t) -> 'c -> 'a list -> ('c * 'b list) t

(** The result, or the first error raised again. For the driver, until it
    chains the stages by their values. *)
val or_raise : 'a t -> 'a
