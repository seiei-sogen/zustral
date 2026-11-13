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

module Syntax = struct
  let ( let* ) = Result.bind
end
open Syntax

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
            [traverse f rest] here and put its errors after these. The
            callers already take a list of errors, but this function is not
            the only place that has to change:
            - cli/CliEngine.ml (exec_compile) prints the first error of the
              list and drops the rest;
            - lib/CombiningPass.ml (combine) chains the traversal of the
              interface and the one of the body with [let*], so the body is
              not looked at after an error in the interface;
            - BodyExtractionPass.extract_bodies and CodeGen.gen_module go
              through the declarations inside one [Outcome.catch], which
              ends at the first error. They do not call this function. *)
         Error errors)

let rec traverse_with_context (f: ('c * 'a) -> ('c * 'b) t) (ctx: 'c) (list: 'a list): ('c * 'b list) t =
  match list with
  | [] ->
     Ok (ctx, [])
  | first :: rest ->
     let* (ctx, first) = f (ctx, first) in
     map (fun (ctx, rest) -> (ctx, first :: rest)) (traverse_with_context f ctx rest)
