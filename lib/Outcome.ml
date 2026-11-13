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

let bind: 'a t -> ('a -> 'b t) -> 'b t = Result.bind

let map: ('a -> 'b) -> 'a t -> 'b t = Result.map

let rec traverse (f: 'a -> 'b t) (list: 'a list): 'b list t =
  match list with
  | [] ->
     Ok []
  | first :: rest ->
     (match f first with
      | Ok first ->
         map (fun rest -> first :: rest) (traverse f rest)
      | Error errors ->
         (* To report the errors of every element (issue #57), go on with
            [traverse f rest] here and put its errors after these. Only this
            function changes: the callers already take a list of errors. *)
         Error errors)

let rec traverse_with_context (f: ('c * 'a) -> ('c * 'b) t) (ctx: 'c) (list: 'a list): ('c * 'b list) t =
  match list with
  | [] ->
     Ok (ctx, [])
  | first :: rest ->
     bind (f (ctx, first)) (fun (ctx, first) ->
         map (fun (ctx, rest) -> (ctx, first :: rest)) (traverse_with_context f ctx rest))

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
