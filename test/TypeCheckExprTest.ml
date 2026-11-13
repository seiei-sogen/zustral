(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Austral_core
open Austral_core.Identifier
open Austral_core.Common
open Austral_core.Type
open Austral_core.Error
open Austral_core.Stages.AstDB
open Austral_core.Stages.Tast
module TypeCheckExpr = Austral_core.TypeCheckExpr

let example: module_name = make_mod_name "Example"

let named (name: string): qident =
  make_qident (example, make_ident name, make_ident name)

let int32: ty = Integer (Signed, Width32)

(* The context of an expression in the body of a function of this module:

       module Example is
           generic [T: Free]
           function make(): T;
       end module.

   with the local variables `x: Int32` and `isZero: Fn[Int32]: Bool`. *)
let ctx: TypeCheckExpr.expr_ctx =
  let (env, mod_id) =
    Env.add_module Env.empty_env {
        name = example;
        imported_instances = InstanceIdSet.InstanceIdSet.empty
      }
  in
  let t: TypeParameter.type_parameter =
    TypeParameter.make_typaram (make_ident "T", FreeUniverse, named "make", [])
  in
  let (env, _) =
    Env.register NoSpan env (EnvTypes.NewFunction {
        mod_id;
        vis = VisPublic;
        name = make_ident "make";
        docstring = Docstring "";
        typarams = TypeParameters.typarams_from_list Error.NoSpan [t];
        value_params = [];
        rt = TyVar (TypeParameter.typaram_to_tyvar t);
        impl = BodyNotAttached
      })
  in
  let lexenv: LexEnv.lexenv =
    LexEnv.push_vars LexEnv.empty_lexenv [
        (make_ident "x", int32, LexEnv.VarLocal Immutable);
        (make_ident "isZero", FnPtr ([int32], Boolean), LexEnv.VarLocal Immutable)
      ]
  in
  TypeCheckExpr.make_ctx example env RegionMap.empty_region_map TypeParameters.empty_typarams lexenv

let three_hundred: NumberLiteral.int_literal = NumberLiteral.of_decimal "300"

let call_make: aexpr = FunctionCall (named "make", Positional [])

(* The error the function stops with. *)
let error_of (f: unit -> texpr): error option =
  match f () with
  | _ -> None
  | exception Austral_error (AustralError { error; _ }) -> Some error

let test_literal _ =
  let literal: aexpr = IntConstant three_hundred in
  assert_equal (TIntConstant three_hundred) (TypeCheckExpr.infer ctx literal);
  (* A literal that is checked stays a literal: it has to fit the type, and
     nothing records which type it was checked against. *)
  assert_equal (TIntConstant three_hundred) (TypeCheckExpr.check ctx literal (Integer (Unsigned, Width16)));
  assert_equal
    (Some (Type (IntegerDoesNotFit { literal = three_hundred; ty = Integer (Unsigned, Width8) })))
    (error_of (fun () -> TypeCheckExpr.check ctx literal (Integer (Unsigned, Width8))));
  assert_equal
    (Some (Type (TypeMismatch { expected = Boolean; actual = Integer (Signed, Width32) })))
    (error_of (fun () -> TypeCheckExpr.check ctx literal Boolean))

let test_variable _ =
  let x: aexpr = Variable (named "x") in
  assert_equal (TLocalVar (make_ident "x", int32)) (TypeCheckExpr.infer ctx x);
  assert_equal (TLocalVar (make_ident "x", int32)) (TypeCheckExpr.check ctx x int32);
  assert_equal
    (Some (Type (TypeMismatch { expected = Boolean; actual = int32 })))
    (error_of (fun () -> TypeCheckExpr.check ctx x Boolean))

let test_function_pointer_call _ =
  let call: aexpr = FunctionCall (named "isZero", Positional [Variable (named "x")]) in
  let expected: texpr = TFptrCall (make_ident "isZero", [TImplicitCast (TLocalVar (make_ident "x", int32), int32)], Boolean) in
  assert_equal expected (TypeCheckExpr.infer ctx call);
  assert_equal expected (TypeCheckExpr.check ctx call Boolean)

(* Nothing in `make()` says what `T` is. Only the type the call is checked
   against does. *)
let test_call_that_needs_the_expected_type _ =
  assert_equal
    (Some (Type (UnconstrainedGenericFunction (make_ident "make"))))
    (error_of (fun () -> TypeCheckExpr.infer ctx call_make));
  (match TypeCheckExpr.check ctx call_make int32 with
   | TFuncall (_, name, [], rt, _) ->
      assert_bool "the callable is make" (equal_qident name (named "make"));
      assert_equal int32 rt
   | other ->
      assert_failure ("not a call of make: " ^ show_texpr other))

(* The expected type goes no further than the expression it is given to: the
   operand of a comparison is inferred. *)
let test_expected_type_stops_at_operands _ =
  let comparison: aexpr = Comparison (Equal, call_make, Variable (named "x")) in
  assert_equal
    (Some (Type (UnconstrainedGenericFunction (make_ident "make"))))
    (error_of (fun () -> TypeCheckExpr.check ctx comparison Boolean))

(* An annotation gives a literal its type with a cast, which a place that
   expects a type does not. `e : T` and the type of a `let` are the same
   annotation. *)
let test_annotation _ =
  let literal: aexpr = IntConstant three_hundred
  and nat16: ty = Integer (Unsigned, Width16) in
  let nat16_spec: qtypespec =
    let name: identifier = make_ident (BuiltInNames.type_name BuiltInNames.Nat16) in
    Stages.Ast.QTypeSpecifier (make_qident (BuiltIn.pervasive_module_name, name, name), [])
  in
  assert_equal (TCast (TIntConstant three_hundred, nat16)) (TypeCheckExpr.check_annotated ctx literal nat16);
  assert_equal (TCast (TIntConstant three_hundred, nat16)) (TypeCheckExpr.infer ctx (Typecast (literal, nat16_spec)));
  assert_equal
    (Some (Type (IntegerDoesNotFit { literal = three_hundred; ty = Integer (Unsigned, Width8) })))
    (error_of (fun () -> TypeCheckExpr.check_annotated ctx literal (Integer (Unsigned, Width8))));
  (* An expression that is not a literal has to have the type, and stays as it is. *)
  let x: aexpr = Variable (named "x") in
  assert_equal (TLocalVar (make_ident "x", int32)) (TypeCheckExpr.check_annotated ctx x int32);
  assert_equal
    (Some (Type (CastInvalid { target = Boolean; source = int32 })))
    (error_of (fun () -> TypeCheckExpr.check_annotated ctx x Boolean))

let suite =
  "TypeCheckExpr" >::: [
      "a literal" >:: test_literal;
      "a variable" >:: test_variable;
      "a call through a function pointer" >:: test_function_pointer_call;
      "a call that needs the expected type" >:: test_call_that_needs_the_expected_type;
      "the expected type stops at operands" >:: test_expected_type_stops_at_operands;
      "an annotation" >:: test_annotation
    ]

let _ = run_test_tt_main suite
