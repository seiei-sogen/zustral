(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Stages

module C = Combined
module SC = SmallCombined

let te (supply: Supply.t) (expr: Ast.aexpr): AstDB.aexpr =
  DesugarBorrows.transform_expr supply (DesugarPaths.transform_expr (LiftControlPass.transform expr))

let ts (supply: Supply.t) (stmt: Ast.astmt): AstDB.astmt * Supply.t =
  let (stmt, supply) = LiftControlPass.lift supply stmt in
  DesugarBorrows.transform_stmt supply (DesugarPaths.transform_stmt stmt)

let copy_combined_method_def (supply: Supply.t) (C.CMethodDef (id, tp, qp, qt, ds, ast)) =
  let (ast, supply) = ts supply ast in
  (SC.CMethodDef (id, tp, qp, qt, ds, ast), supply)

let desugar_decl (supply: Supply.t) (decl: C.combined_definition): SC.combined_definition * Supply.t =
  match decl with
  | C.CConstant (s, v, id, qt, ae, ds) -> (SC.CConstant (s, v, id, qt, te supply ae, ds), supply)
  | C.CRecord (s, tv, id, tp, u, ql, ds) -> (SC.CRecord (s, tv, id, tp, u, ql, ds), supply)
  | C.CUnion (s, tv, id, tp, u, qc, ds) -> (SC.CUnion (s, tv, id, tp, u, qc, ds), supply)
  | C.CFunction (s, v, id, tp, qp, qt, ast, ds, pl) ->
     let (ast, supply) = ts supply ast in
     (SC.CFunction (s, v, id, tp, qp, qt, ast, ds, pl), supply)
  | C.CTypeclass (s, v, id, tp, methods, ds) -> (SC.CTypeclass (s, v, id, tp, methods, ds), supply)
  | C.CInstance (s, v, qi, tp, qt, cmd, ds) ->
     let (cmd, supply) = Supply.map copy_combined_method_def supply cmd in
     (SC.CInstance (s, v, qi, tp, qt, cmd, ds), supply)

let desugar (supply: Supply.t) (md: ReturnCheck.returning_module): (SC.combined_module * Supply.t) Outcome.t =
  let (C.CombinedModule { name; kind; interface_docstring; interface_imports; body_docstring; body_imports; decls }) = ReturnCheck.as_combined_module md in
  Outcome.map
    (fun (supply, decls) -> (SC.CombinedModule { name; kind; interface_docstring; interface_imports; body_docstring; body_imports; decls }, supply))
    (Outcome.traverse_with_context
       (fun (supply, decl) -> Outcome.catch (fun () -> let (decl, supply) = desugar_decl supply decl in (supply, decl)))
       supply
       decls)
