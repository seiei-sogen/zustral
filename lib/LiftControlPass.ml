(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Stages
open Span

(* A statement takes the name of its temporary before its sub-statements take
   theirs, and the sub-statements go from the last to the first: the `else`
   branch before the `then` branch, the rest of a block before its first
   statement. The names are in the generated C, so this order stays. It is the
   order OCaml evaluated the arguments of the constructors in, when the names
   came from a global counter. *)
let rec lift (supply: Supply.t) (stmt: Ast.astmt): AstLC.astmt * Supply.t =
  match stmt with
  | Ast.ASkip span ->
     (AstLC.ASkip span, supply)
  | Ast.ALet (span, m, id, qt, name, value, s) ->
     let (s, supply) = lift supply s in
     (AstLC.ALet (span, m, id, qt, AstLC.ABlock (empty_span, AstLC.AInitialAssign (name, qt, transform value), s)), supply)
  | Ast.ADestructure (span, m, qbl, e, s) ->
     let (s, supply) = lift supply s in
     (AstLC.ADestructure (span, m, qbl, transform e, s), supply)
  | Ast.AAssign (span, lv, e) ->
     let (tmp, supply): identifier * Supply.t = fresh_ident supply in
     let e = transform e in
     let lv = transform_lvalue lv in
     (AstLC.ABlock (
         empty_span,
         AstLC.LetTmp (
             tmp,
             e
           ),
         AstLC.AAssign (span, lv, Temporary tmp)
       ), supply)
  | Ast.AIf (span, e, s1, s2) ->
     let (tmp, supply): identifier * Supply.t = fresh_ident supply in
     let e = transform e in
     let (s2, supply) = lift supply s2 in
     let (s1, supply) = lift supply s1 in
     (AstLC.ABlock (
         empty_span,
         AstLC.LetTmp (
             tmp,
             e
           ),
         AstLC.AIf (
             span,
             Temporary tmp,
             s1,
             s2
           )
       ), supply)
  | Ast.AWhen (span, e, s) ->
     let (tmp, supply): identifier * Supply.t = fresh_ident supply in
     let e = transform e in
     let (s, supply) = lift supply s in
     (AstLC.ABlock (
         empty_span,
         AstLC.LetTmp (
             tmp,
             e
           ),
         AstLC.AWhen (
             span,
             Temporary tmp,
             s
           )
       ), supply)
  | Ast.ACase (span, e, awl) ->
     let (tmp, supply): identifier * Supply.t = fresh_ident supply in
     let e = transform e in
     let (awl, supply) = Supply.map transform_abstract_when supply awl in
     (AstLC.ABlock (
         empty_span,
         AstLC.LetTmp (
             tmp,
             e
           ),
         AstLC.ACase (
             span,
             Temporary tmp,
             awl
           )
       ), supply)
  | Ast.AWhile (span, e, s) ->
     let (tmp, supply): identifier * Supply.t = fresh_ident supply in
     let e = transform e in
     let (s, supply) = lift supply s in
     (AstLC.ABlock (
         empty_span,
         AstLC.LetTmp (
             tmp,
             e
           ),
         AstLC.AWhile (
             span,
             Temporary tmp,
             AstLC.ABlock (
                 empty_span,
                 s,
                 AssignTmp (tmp, e)
               )
           )
       ), supply)
  | Ast.AFor {span; name; initial; final; body} ->
     let (tmp_initial, supply): identifier * Supply.t = fresh_ident supply in
     let (tmp_final, supply): identifier * Supply.t = fresh_ident supply in
     let idx =
       Ast.QTypeSpecifier (
           make_qident (
               BuiltIn.pervasive_module_name,
               make_ident (BuiltInNames.type_name BuiltInNames.Index),
               make_ident (BuiltInNames.type_name BuiltInNames.Index)
             ),
           []
         )
     in
     let (body, supply) = lift supply body in
     (AstLC.ABlock (
         empty_span,
         AstLC.LetTmp (
             tmp_initial,
             Typecast (transform initial, idx)
           ),
         AstLC.ABlock (
             empty_span,
             AstLC.LetTmp (
                 tmp_final,
                 Typecast (transform final, idx)
               ),
             AstLC.AFor {
                 span;
                 name;
                 initial=Temporary tmp_initial;
                 final=Temporary tmp_final;
                 body
               }
           )
       ), supply)
  | Ast.ABorrow {span; original; rename; region; body; mode} ->
     let (body, supply) = lift supply body in
     (AstLC.ABorrow {span; original; rename; region; body; mode}, supply)
  | Ast.ABlock (span, s1, s2) ->
     let (s2, supply) = lift supply s2 in
     let (s1, supply) = lift supply s1 in
     (AstLC.ABlock (span, s1, s2), supply)
  | Ast.ADiscarding (span, e) ->
     (AstLC.ADiscarding (span, transform e), supply)
  | Ast.AReturn (span, e) ->
     (AstLC.AReturn (span, transform e), supply)

and transform (expr: Ast.aexpr): AstLC.aexpr =
  match expr with
  | Ast.NilConstant ->
    AstLC.NilConstant
  | Ast.BoolConstant b ->
    AstLC.BoolConstant b
  | Ast.IntConstant s ->
    AstLC.IntConstant s
  | Ast.FloatConstant s ->
    AstLC.FloatConstant s
  | Ast.StringConstant es ->
    AstLC.StringConstant es
  | Ast.Variable q ->
    AstLC.Variable q
  | Ast.FunctionCall (q, al) ->
    AstLC.FunctionCall (q, transform_arglist al)
  | Ast.ArithmeticExpression (ao, e1, e2) ->
    AstLC.ArithmeticExpression (ao, transform e1, transform e2)
  | Ast.Comparison (co, e1, e2) ->
    AstLC.Comparison (co, transform e1, transform e2)
  | Ast.Conjunction (e1, e2) ->
    AstLC.Conjunction (transform e1, transform e2)
  | Ast.Disjunction (e1, e2) ->
    AstLC.Disjunction (transform e1, transform e2)
  | Ast.Negation e ->
    AstLC.Negation (transform e)
  | Ast.IfExpression (e1, e2, e3) ->
    AstLC.IfExpression (transform e1, transform e2, transform e3)
  | Ast.Path (head, pel) ->
    AstLC.Path (head, transform_path pel)
  | Ast.RefPath (head, rpel) ->
    AstLC.RefPath (head, transform_path rpel)
  | Ast.Embed (qt, s, el) ->
    AstLC.Embed (qt, s, List.map transform el)
  | Ast.Deref e ->
    AstLC.Deref (transform e)
  | Ast.Typecast (e, qt) ->
    AstLC.Typecast (transform e, qt)
  | Ast.SizeOf qt ->
    AstLC.SizeOf qt
  | Ast.BorrowExpr (bm, q) ->
    AstLC.BorrowExpr (bm, q)
  | Ast.Reborrow q ->
    AstLC.Reborrow q

and transform_abstract_when supply aw =
  match aw with
  | Ast.AbstractWhen (id, qbl, s) ->
     let (s, supply) = lift supply s in
     (AstLC.AbstractWhen (id, qbl, s), supply)

and transform_arglist al =
  match al with
  | Ast.Positional el -> AstLC.Positional (List.map transform el)
  | Ast.Named idel -> AstLC.Named (List.map (fun (id, e) -> (id, transform e)) idel)

and transform_lvalue (Ast.LValue (head, pel)): AstLC.lvalue =
  AstLC.LValue (head, transform_path pel)

and transform_path (elems: Ast.path_elem list): AstLC.path_elem list =
  List.map transform_elem elems

and transform_elem (elem: Ast.path_elem): AstLC.path_elem =
  match elem with
  | Ast.SlotAccessor name ->
    AstLC.SlotAccessor name
  | Ast.PointerSlotAccessor name ->
    AstLC.PointerSlotAccessor name
  | Ast.ArrayIndex expr ->
    AstLC.ArrayIndex (transform expr)
