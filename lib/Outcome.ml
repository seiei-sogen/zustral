(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Error

type 'a t = ('a, austral_error list) result

let catch (f: unit -> 'a): 'a t =
  try
    Ok (f ())
  with Austral_error error ->
    Error [error]

let or_raise (outcome: 'a t): 'a =
  match outcome with
  | Ok value ->
     value
  | Error errors ->
     let error: austral_error =
       match errors with
       | first :: _ ->
          first
       | [] ->
          (* No stage returns a failure without an error. *)
          AustralError { module_name = None; error = Internal "A stage failed without an error."; span = None; source_ctx = None }
     in
     raise (Austral_error error)
