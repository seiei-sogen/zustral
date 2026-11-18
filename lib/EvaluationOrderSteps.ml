(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Common
open Type
open Span
open Stages.Tast

type step =
  | EvaluateIntoNewTemporary of identifier * ty * texpr
  | DeclareTemporary of identifier * ty
  | EvaluateOnOnePath of tstmt

(* The statements this stage makes up have the empty span, as those of
   LiftControlPass do: no stage after this one reports an error at a
   statement. *)
let rec statement_after_steps (steps: step list) (stmt: tstmt): tstmt =
  match steps with
  | [] ->
     stmt
  | EvaluateIntoNewTemporary (name, ty, value) :: later ->
     TBlock (empty_span, TLetTmp (name, ty, value), statement_after_steps later stmt)
  | DeclareTemporary (name, ty) :: later ->
     (* The steps after the declaration and the statement are the scope of the
        temporary, which is what the body of a `let` is. *)
     TLet (empty_span, Mutable, name, ty, statement_after_steps later stmt)
  | EvaluateOnOnePath conditional :: later ->
     TBlock (empty_span, conditional, statement_after_steps later stmt)
