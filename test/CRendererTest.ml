(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Zustral_core.CRenderer
open Zustral_core.Escape
module CIdent = Zustral_core.CIdent

let r_e = render_expr
let test_render_bool _ =
  assert_equal (r_e (CBool true)) "true";
  assert_equal (r_e (CBool false)) "false"

let test_render_string _ =
  assert_equal (r_e (CString  (escape_string Zustral_core.Error.NoSpan "abcd"))) "\"abcd\"";
  assert_equal (r_e (CString (escape_string Zustral_core.Error.NoSpan "\""))) "\"\\\"\"";
  assert_equal (r_e (CString (escape_string Zustral_core.Error.NoSpan "\"\"\""))) "\"\\\"\\\"\\\"\""

(* Which names get the reserved-word escape. The cases that issue #92 calls
   bugs are here as they are today. *)
let test_render_identifier _ =
  assert_equal (r_e (CVar (CIdent.generated "decl_1"))) "decl_1";
  assert_equal (r_e (CVar (CIdent.zustral "x"))) "x";
  assert_equal (r_e (CVar (CIdent.zustral "double"))) "au_ckw_double";
  assert_equal (r_e (CVar (CIdent.zustral "doubled"))) "au_ckw_doubled";
  assert_equal (r_e (CStructAccessor (CVar (CIdent.zustral "x"), CIdent.zustral_unescaped "double"))) "(x).double";
  assert_equal (r_e (CFuncall (CIdent.external_name "fork", []))) "fork()";
  assert_equal (r_e (CFuncall (CIdent.external_escaped "fork", []))) "au_ckw_fork()"
  
  
  let suite =
    "CliUtil" >::: [
        "render_bool" >:: test_render_bool;
        "render_string" >:: test_render_string;
        "render_identifier" >:: test_render_identifier
      ]
  
  let _ = run_test_tt_main suite
  