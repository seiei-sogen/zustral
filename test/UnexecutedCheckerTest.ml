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
open Austral_core.Stages.Tast
module A = Austral_core.Stages.AstDB
module Linked = Austral_core.Stages.Linked

(* The type checker and the checks around it, given values that the stages
   before them never make: a syntax tree of a shape the lifting and desugaring
   passes do not produce, a typed tree that uses a variable as the type checker
   would not let it, an expression kind in a place no program puts it. *)

open Outcome.Syntax

let get (outcome: 'a Outcome.t): 'a =
  match outcome with
  | Ok v -> v
  | Error (first, rest) ->
     assert_failure (String.concat "\n" (List.map render_error_to_plain (first :: rest)))

(* The one error of a failed result. *)
let the_error (outcome: 'a Outcome.t): error =
  match outcome with
  | Ok _ -> assert_failure "the stage did not fail"
  | Error (AustralError { error; _ }, []) -> error
  | Error _ -> assert_failure "the stage failed with more than one error"

(* The error a function stops with. *)
let raised (f: unit -> 'a): error =
  match f () with
  | _ -> assert_failure "no error was raised"
  | exception Austral_error (AustralError { error; _ }) -> error

(* The stages before the type checker, on a module body. *)
let front (env: Env.env) (code: string): (Env.env * Linked.linked_module) Outcome.t =
  let* cb = ParserInterface.parse_module_body code "Test.aum" in
  let (Cst.ConcreteModuleBody (mn, kind, doc, imports, decls)) = cb in
  let cb = Cst.ConcreteModuleBody (mn, kind, doc, BuiltIn.pervasive_imports :: imports, decls) in
  let* combined = CombiningPass.body_as_combined env cb in
  let* returning = ReturnCheck.check_ends_in_return combined in
  let* (env, small) = Env.with_supply env (fun supply -> DesugaringPass.desugar supply returning) in
  ExtractionPass.extract env small

(* The declarations of Austral.Pervasive, which the module below uses. *)
let pervasive: Env.env Lazy.t =
  lazy (get (
    let* ci = ParserInterface.parse_module_int BuiltInModules.pervasive_interface_source "" in
    let* cb = ParserInterface.parse_module_body BuiltInModules.pervasive_body_source "" in
    let* combined = CombiningPass.combine Env.empty_env ci cb in
    let* returning = ReturnCheck.check_ends_in_return combined in
    let* (env, small) = Env.with_supply Env.empty_env (fun supply -> DesugaringPass.desugar supply returning) in
    let* (env, _) = ExtractionPass.extract env small in
    Ok env))

let test_mod: module_name = make_mod_name "Test"

let q (name: string): qident = make_qident (test_mod, make_ident name, make_ident name)

let i = make_ident

let builtin_spec (t: BuiltInNames.builtin_type): Stages.Ast.qtypespec =
  let name: identifier = make_ident (BuiltInNames.type_name t) in
  Stages.Ast.QTypeSpecifier (make_qident (BuiltIn.pervasive_module_name, name, name), [])

let int32: ty = Integer (Signed, Width32)
let int32_spec: Stages.Ast.qtypespec = builtin_spec BuiltInNames.Int32
let static: ref_region = RefRegion Region.static_region

let source: string = {|
module body Test is
    constant c: Int32 := 0;

    record Box: Free is
        value: Int32;
    end;

    union Shape: Free is
        case Dot;
    end;

    typeclass Describe(T: Free) is
        method describe(value: T): Int32;
    end;

    typeclass Show(T: Type) is
        method show(value: T): Int32;
    end;

    instance Describe(Int32) is
        method describe(value: Int32): Int32 is
            return value;
        end;
    end;

    function f(): Int32 is
        return 0;
    end;
end module body.
|}

type world = {
    env: Env.env;
    linked: Linked.linked_module;
    mod_id: Id.mod_id;
    f_id: Id.function_id;
    box_id: Id.record_id;
    shape_id: Id.union_id;
    describe_id: Id.typeclass_id;
    meth_id: Id.ins_meth_id;
    (* A type variable `T: Free` that implements Describe. *)
    tyvar: ty;
  }

let world: world Lazy.t =
  lazy (
    let (env, linked) = get (front (Lazy.force pervasive) source) in
    let (Linked.LinkedModule { decls; mod_id; _ }) = linked in
    let pick f = match List.find_map f decls with Some x -> x | None -> assert_failure "missing declaration" in
    let f_id = pick (function Linked.LFunction (id, _, _, _, _, _, _, _, _) -> Some id | _ -> None)
    and box_id = pick (function Linked.LRecord (id, _, _, _, _, _, _) -> Some id | _ -> None)
    and shape_id = pick (function Linked.LUnion (id, _, _, _, _, _, _) -> Some id | _ -> None)
    and describe_id = pick (function Linked.LTypeclass (id, _, n, _, _, _) when n = i "Describe" -> Some id | _ -> None)
    and meth_id = pick (function Linked.LInstance (_, _, _, _, _, [Linked.LMethodDef (id, _, _, _, _, _)], _) -> Some id | _ -> None) in
    let tyvar: ty = TyVar (TypeVariable (i "T", FreeUniverse, q "f", [make_sident test_mod (i "Describe")])) in
    (* Constants of types no constant of a program has. *)
    let constant env name ty =
      fst (Env.register NoSpan env (EnvTypes.NewConstant { mod_id; vis = VisPublic; name = i name; ty; docstring = Docstring "" }))
    in
    let box: ty = NamedType (q "Box", Id.RecordTypeId box_id, [], FreeUniverse) in
    let env = constant env "generic" tyvar in
    let env = constant env "refref" (ReadRef (ReadRef (box, static), static)) in
    { env; linked; mod_id; f_id; box_id; shape_id; describe_id; meth_id; tyvar })

let box (): ty =
  let w = Lazy.force world in
  NamedType (q "Box", Id.RecordTypeId w.box_id, [], FreeUniverse)

(* LiteralTyping *)

let one: NumberLiteral.int_literal = NumberLiteral.of_decimal "1"
let half: NumberLiteral.float_literal = NumberLiteral.of_float "0.5"

let every_aexpr: A.aexpr list =
  let x = A.Variable (q "x") in
  [A.NilConstant; A.BoolConstant true; A.IntConstant one; A.FloatConstant half;
   A.StringConstant (Escape.escape_string NoSpan "s"); x; A.Temporary (i "t");
   A.FunctionCall (q "f", A.Positional []); A.ArithmeticExpression (Add, x, x);
   A.Comparison (Equal, x, x); A.Conjunction (x, x); A.Disjunction (x, x); A.Negation x;
   A.IfExpression (x, x, x); A.Embed (int32_spec, "1", []); A.Deref x; A.Typecast (x, int32_spec);
   A.SizeOf int32_spec; A.SlotAccessor (x, i "value"); A.PointerSlotAccessor (x, i "value");
   A.ArrayIndex (x, x)]

let test_numeric_literal _ =
  List.iter (fun e ->
      let expected = (match e with A.IntConstant _ | A.FloatConstant _ -> true | _ -> false) in
      assert_equal expected (LiteralTyping.is_numeric_literal e))
    every_aexpr

(* One expression of each kind the type checker makes. *)
let every_texpr (): texpr list =
  let w = Lazy.force world in
  let x = TLocalVar (i "x", int32) in
  [TNilConstant; TBoolConstant true; TIntConstant one; TFloatConstant half;
   TStringConstant (Escape.escape_string NoSpan "s"); TConstVar (q "c", int32);
   TParamVar (i "p", int32); x; TFunVar (w.f_id, FnPtr ([], int32), TypeBindings.empty_bindings);
   TTemporary (i "t", int32); TFuncall (w.f_id, q "f", [], int32, TypeBindings.empty_bindings);
   TMethodCall (w.meth_id, q "describe", TypeParameters.empty_typarams, [x], int32, TypeBindings.empty_bindings);
   TVarMethodCall { source_module_name = test_mod; typeclass_id = w.describe_id; params = [];
                    method_name = q "describe"; args = []; dispatch_ty = w.tyvar; rt = int32;
                    bindings = TypeBindings.empty_bindings };
   TFptrCall (i "fp", [], int32); TCast (x, int32); TImplicitCast (x, int32);
   TComparison (Equal, x, x); TConjunction (x, x); TDisjunction (x, x); TNegation x;
   TIfExpression (x, x, x); TRecordConstructor (box (), [(i "value", x)]);
   TUnionConstructor { union_id = w.shape_id; type_name = q "Shape"; tyargs = []; universe = FreeUniverse;
                       case_name = i "Dot"; args = [] };
   TSlotAccessor (x, i "value", int32); TPointerSlotAccessor (x, i "value", int32);
   TArrayIndex (x, x, int32, static, ReadBorrow); TSpanIndex (x, x, int32, static, ReadBorrow);
   TEmbed (int32, "1", []); TDeref (x, int32); TSizeOf int32]

let int_literals: texpr list =
  [TIntConstant one; TCast (TIntConstant one, int32); TImplicitCast (TIntConstant one, int32)]

let float_literals: texpr list =
  [TFloatConstant half; TCast (TFloatConstant half, DoubleFloat); TImplicitCast (TFloatConstant half, DoubleFloat)]

(* In a place that expects a number type, a literal of that kind is typed by
   the place, also under a cast, and nothing else is. *)
let test_literal_matches _ =
  List.iter (fun e ->
      assert_equal (List.mem e int_literals) (LiteralTyping.literal_matches NoSpan int32 e);
      assert_equal (List.mem e float_literals) (LiteralTyping.literal_matches NoSpan DoubleFloat e))
    (every_texpr () @ int_literals @ float_literals)

(* An annotation types a bare literal only. *)
let test_annotation_types_literal _ =
  List.iter (fun e ->
      assert_equal (e = TIntConstant one) (LiteralTyping.annotation_types_literal NoSpan int32 e);
      assert_equal (e = TFloatConstant half) (LiteralTyping.annotation_types_literal NoSpan SingleFloat e))
    (every_texpr () @ int_literals @ float_literals)

(* The signed widths of sizes and indices: the type parser makes both
   unsigned, the bounds are there all the same. *)
let test_signed_size_widths _ =
  let fits ty n = LiteralTyping.literal_matches NoSpan ty (TIntConstant (NumberLiteral.of_decimal n)) in
  let does_not_fit ty n =
    let literal = NumberLiteral.of_decimal n in
    assert_equal (Type (IntegerDoesNotFit { literal; ty }))
      (raised (fun () -> LiteralTyping.literal_matches NoSpan ty (TIntConstant literal)))
  in
  let byte = Integer (Signed, WidthByteSize) and index = Integer (Signed, WidthIndex) in
  assert_bool "-128 is a signed byte size" (fits byte "-128");
  does_not_fit byte "128";
  assert_bool "2^63-1 is a signed index" (fits index "9223372036854775807");
  does_not_fit index "9223372036854775808"

(* CstUtil *)

let test_pragma_arguments _ =
  let check name pragma =
    List.iter (fun args ->
        assert_equal (Declaration (PragmaArgumentError pragma))
          (raised (fun () -> CstUtil.make_pragma (i name) args)))
      [Cst.ConcretePositionalArgs []; Cst.ConcreteNamedArgs [];
       Cst.ConcreteNamedArgs [(i BuiltInNames.external_name_argument, Cst.CNilConstant Span.empty_span)]]
  in
  check BuiltInNames.foreign_import_pragma_name ForeignImport;
  check BuiltInNames.foreign_export_pragma_name ForeignExport

(* LexEnv *)

let test_undeclared_temporary _ =
  assert_equal (Internal "Temporary variable does not exist in lexenv: t")
    (raised (fun () -> LexEnv.get_temporary_type NoSpan LexEnv.empty_lexenv (i "t")))

(* LinearityCheck *)

let sp: Span.span = Span.empty_span

let linear: ty = TyVar (TypeVariable (i "L", LinearUniverse, q "f", []))

let check_linearity (params: value_parameter list) (body: tstmt): unit Outcome.t =
  let w = Lazy.force world in
  Outcome.map ignore
    (LinearityCheck.check_module_linearity
       (TypedModule (test_mod, [TFunction (w.f_id, VisPublic, i "f", TypeParameters.empty_typarams, params, int32, body, Docstring "")])))

(* A `let` of the name of a parameter: the type checker rejects it. *)
let test_name_declared_twice _ =
  assert_equal (Internal "An entry exists in the state table with this name: x")
    (the_error (check_linearity [ValueParameter (i "x", int32)] (TLet (sp, Immutable, i "x", int32, TSkip sp))))

let t: identifier = i "t"
let let_t: tstmt = TLetTmp (t, linear, TNilConstant)
let use_t: tstmt = TDiscarding (sp, TTemporary (t, linear))
let assign_t: tstmt = TAssignTmp (t, TNilConstant)

let borrow_t (mode: borrow_stmt_kind) (body: tstmt): tstmt =
  TBorrow { span = sp; original = t; rename = i "r"; region = i "R"; orig_type = linear;
            ref_type = ReadRef (linear, static); body; mode }

let rec block (stmts: tstmt list): tstmt =
  match stmts with
  | [s] -> s
  | s :: rest -> TBlock (sp, s, block rest)
  | [] -> TSkip sp

(* Assigning a linear temporary that holds a value, also a borrowed one, loses
   that value. Assigning one that was consumed gives it a value to consume
   again. The type checker makes an AssignTmp only for the condition of a
   `while`, which is a Bool. *)
let test_assign_temporary _ =
  let unconsumed = Linearity (AssignToUnconsumed t) in
  assert_equal unconsumed (the_error (check_linearity [] (block [let_t; assign_t])));
  assert_equal unconsumed (the_error (check_linearity [] (block [let_t; borrow_t Read assign_t])));
  assert_equal unconsumed (the_error (check_linearity [] (block [let_t; borrow_t Write assign_t])));
  assert_equal (Ok ()) (check_linearity [] (block [let_t; use_t; assign_t; use_t]));
  assert_equal (Linearity (AlreadyConsumed t)) (the_error (check_linearity [] (block [let_t; use_t; use_t])));
  (* A name not in the table is not a variable whose use is checked. *)
  assert_equal (Ok ()) (check_linearity [] (TAssignTmp (i "elsewhere", TNilConstant)))

(* TypeCheckExpr *)

let ctx_with (vars: (string * ty) list): TypeCheckExpr.expr_ctx =
  let w = Lazy.force world in
  let lexenv = LexEnv.push_vars NoSpan LexEnv.empty_lexenv
                 (List.map (fun (n, ty) -> (i n, ty, LexEnv.VarLocal Immutable)) vars) in
  TypeCheckExpr.make_ctx NoSpan test_mod w.env RegionMap.empty_region_map TypeParameters.empty_typarams lexenv

let ctx (): TypeCheckExpr.expr_ctx = ctx_with [("x", int32); ("t", int32); ("b", Boolean)]

let var (n: string): A.aexpr = A.Variable (q n)

(* Expressions an expected type does not reach are checked as they are
   inferred. A Temporary and a PointerSlotAccessor never stand where a type is
   expected in a program. *)
let test_checked_as_inferred _ =
  let ctx = ctx () in
  List.iter (fun (e, ty) -> assert_equal (TypeCheckExpr.infer ctx e) (TypeCheckExpr.check ctx e ty))
    [(A.Temporary t, int32); (A.Comparison (Equal, var "x", var "x"), Boolean);
     (A.Conjunction (var "b", var "b"), Boolean); (A.Disjunction (var "b", var "b"), Boolean);
     (A.Negation (var "b"), Boolean)];
  List.iter (fun e -> assert_equal (raised (fun () -> TypeCheckExpr.infer ctx e)) (raised (fun () -> TypeCheckExpr.check ctx e int32)))
    [A.PointerSlotAccessor (var "x", i "value"); A.ArrayIndex (var "x", A.IntConstant one)]

let non_references (): ty list =
  [Unit; Boolean; int32; SingleFloat; DoubleFloat; box (); RegionTy Region.static_region;
   Span (int32, static); SpanMut (int32, static); linear; Address int32; Pointer int32; FnPtr ([], int32)]

(* `->` on an expression that is not a reference. A program's path starts with
   a borrow, which is one. *)
let test_pointer_slot_of_non_reference _ =
  List.iter (fun ty ->
      assert_equal (Generic (RefTransformNeedsRef ty))
        (raised (fun () -> TypeCheckExpr.infer (ctx_with [("v", ty)]) (A.PointerSlotAccessor (var "v", i "value")))))
    (non_references ())

(* A typeclass whose parameter is a region, which a program cannot declare. *)
let with_region_typeclass (env: Env.env): Env.env =
  let w = Lazy.force world in
  let r: TypeParameter.type_parameter = TypeParameter.make_typaram (i "R", RegionUniverse, q "Regional", []) in
  let (env, typeclass_id) =
    Env.register NoSpan env (EnvTypes.NewTypeClass {
        mod_id = w.mod_id; vis = VisPublic; name = i "Regional"; docstring = Docstring ""; param = r }) in
  let (env, _) =
    Env.register NoSpan env (EnvTypes.NewTypeClassMethod {
        mod_id = w.mod_id; vis = VisPublic; typeclass_id; name = i "regional"; docstring = Docstring "";
        typarams = TypeParameters.empty_typarams;
        value_params = [ValueParameter (i "value", TyVar (TypeParameter.typaram_to_tyvar r))]; rt = int32 }) in
  env

(* A method called with an argument of a type no instance can be for. *)
let test_method_of_no_instance _ =
  let w = Lazy.force world in
  let call (env: Env.env) (meth: string) (ty: ty): error =
    let lexenv = LexEnv.push_var NoSpan LexEnv.empty_lexenv (i "v") ty (LexEnv.VarLocal Immutable) in
    let ctx = TypeCheckExpr.make_ctx NoSpan test_mod env RegionMap.empty_region_map TypeParameters.empty_typarams lexenv in
    raised (fun () -> TypeCheckExpr.infer ctx (A.FunctionCall (q meth, A.Positional [var "v"])))
  in
  let no_instance typeclass ty = Type (NoSuitableInstance { typeclass = i typeclass; ty }) in
  List.iter (fun ty -> assert_equal (no_instance "Describe" ty) (call w.env "describe" ty))
    [ReadRef (int32, static); Span (int32, static); Address int32];
  List.iter (fun ty -> assert_equal (no_instance "Show" ty) (call w.env "show" ty))
    [WriteRef (int32, static); SpanMut (int32, static)];
  let region = RegionTy Region.static_region in
  assert_equal (no_instance "Regional" region) (call (with_region_typeclass w.env) "regional" region)

(* TypingPass *)

let typing ?(env: Env.env option) ?(pragmas: pragma list = []) (params: value_parameter list) (body: A.astmt): typed_decl Outcome.t =
  let w = Lazy.force world in
  let env = Option.value env ~default:w.env in
  let (Linked.LinkedModule m) = w.linked in
  let linked = Linked.LinkedModule { m with decls = [Linked.LFunction (w.f_id, VisPublic, i "f", TypeParameters.empty_typarams, params, int32, body, Docstring "", pragmas)] } in
  let* (_, TypedModule (_, decls)) = Env.with_supply env (fun supply -> TypingPass.augment_module env supply linked) in
  match decls with
  | [d] -> Ok d
  | _ -> assert_failure "not one declaration"

let params: value_parameter list = [ValueParameter (i "x", int32); ValueParameter (i "b", Boolean)]

let values: A.aexpr list = List.filter (function A.Temporary _ -> false | _ -> true) every_aexpr

(* A block is checked in order, unless its second statement assigns a
   temporary: then the assignment, which gives the temporary its type, is
   checked first. No other statement makes the order change, also not an
   assignment of a value that is not a temporary, which the lifting pass
   never leaves. *)
let test_block_order _ =
  let first = A.ADiscarding (sp, var "nowhere") in
  let expected = the_error (typing params first) in
  let seconds: A.astmt list =
    [A.ASkip sp; A.ALet (sp, Immutable, i "y", int32_spec, A.ASkip sp); A.ADestructure (sp, Immutable, [], var "x", A.ASkip sp);
     A.AInitialAssign (q "y", int32_spec, A.IntConstant one); A.AIf (sp, var "b", A.ASkip sp, A.ASkip sp);
     A.AWhen (sp, var "b", A.ASkip sp); A.ACase (sp, var "b", []); A.AWhile (sp, var "b", A.ASkip sp);
     A.AFor { span = sp; name = i "k"; initial = var "x"; final = var "x"; body = A.ASkip sp };
     A.ABlock (sp, A.ASkip sp, A.ASkip sp); A.ADiscarding (sp, var "x"); A.AReturn (sp, var "x");
     A.LetTmp (t, var "x"); A.AssignTmp (t, var "x")]
    @ List.map (fun v -> A.AAssign (sp, var "x", v)) values
    @ List.map (fun v -> A.AAssignVar (sp, q "x", v)) values
  in
  List.iter (fun second -> assert_equal expected (the_error (typing params (A.ABlock (sp, first, second))))) seconds

let body_of (decl: typed_decl): tstmt =
  match decl with
  | TFunction (_, _, _, _, _, _, body, _) -> body
  | _ -> assert_failure "not a function"

(* The value of an assignment that is not a temporary is typed where it
   stands, as it is when it is discarded, and has to have the type of the
   variable. *)
let test_assigned_value_in_place _ =
  let in_let (s: A.astmt): A.astmt = A.ALet (sp, Mutable, i "y", int32_spec, A.ABlock (sp, A.ASkip sp, s)) in
  List.iter (fun v ->
      let assigned = typing params (in_let (A.AAssignVar (sp, q "y", v))) in
      match typing params (in_let (A.ADiscarding (sp, v))) with
      | Error _ as discarded ->
         assert_equal (the_error discarded) (the_error assigned)
      | Ok discarded ->
         (match body_of discarded with
          | TLet (_, _, _, _, TBlock (_, _, TDiscarding (_, v'))) ->
             (match assigned with
              | Ok decl ->
                 (match body_of decl with
                  | TLet (_, _, _, _, TBlock (_, _, TAssignVar (_, name, v''))) ->
                     assert_equal (i "y") (original_name name);
                     assert_equal v' v''
                  | _ -> assert_failure "not an assignment")
              | Error _ ->
                 assert_bool "only a value of another type is not assigned"
                   (TastUtil.get_type NoSpan v' <> int32))
          | _ -> assert_failure "not a discarding statement"))
    values

let all_types (): ty list =
  [Unit; Boolean; int32; SingleFloat; DoubleFloat; box (); RegionTy Region.static_region; ReadRef (int32, static);
   Span (int32, static); SpanMut (int32, static); linear; Address int32; Pointer int32; FnPtr ([], int32)]

(* An assignment whose lvalue is not a write reference. The lvalue of a
   program's assignment is a path, which is a reference. *)
let test_assign_through_non_reference _ =
  List.iter (fun ty ->
      assert_equal (Generic (AssignToNonMutableReference ty))
        (the_error (typing [ValueParameter (i "p", ty)] (A.AAssign (sp, var "p", A.IntConstant one)))))
    (all_types ())

(* A `case` on a value whose type names a declaration that is not a union. *)
let test_case_on_non_union _ =
  let w = Lazy.force world in
  List.iter (fun name ->
      let ty = NamedType (q name, Id.RecordTypeId w.box_id, [], FreeUniverse) in
      assert_equal (Type (CaseNonUnion ty))
        (the_error (typing [ValueParameter (i "p", ty)] (A.ACase (sp, var "p", [])))))
    ["nowhere"; "c"; "Box"; "Dot"; "f"; "Describe"; "describe"]

(* A function whose body the environment holds already, which it does only
   after the type checker has run. *)
let test_function_with_stored_body _ =
  let w = Lazy.force world in
  let env = Env.store_function_body w.env w.f_id (TReturn (sp, TIntConstant one)) in
  assert_equal (TReturn (sp, TIntConstant one)) (body_of (get (typing ~env [] (A.AReturn (sp, A.IntConstant one)))))

(* Pragmas that did not make the function foreign. *)
let test_invalid_pragmas _ =
  List.iter (fun pragmas ->
      assert_equal (Declaration FunInvalidPragmas)
        (the_error (typing ~pragmas [] (A.AReturn (sp, A.IntConstant one)))))
    [[ForeignImportPragma "g"]; [UnsafeModulePragma]; [ForeignExportPragma "g"; UnsafeModulePragma]]

let typing_constant (ty: ty) (value: A.aexpr): typed_decl Outcome.t =
  let w = Lazy.force world in
  let (Linked.LinkedModule m) = w.linked in
  let id = match List.find_map (function Linked.LConstant (id, _, _, _, _, _) -> Some id | _ -> None) m.decls with
    | Some id -> id
    | None -> assert_failure "no constant" in
  let linked = Linked.LinkedModule { m with decls = [Linked.LConstant (id, VisPublic, i "c", ty, value, Docstring "")] } in
  let* (_, TypedModule (_, decls)) = Env.with_supply w.env (fun supply -> TypingPass.augment_module w.env supply linked) in
  match decls with
  | [d] -> Ok d
  | _ -> assert_failure "not one declaration"

(* The value of a constant made from constants of types no program's constant
   has: a method call whose dispatch type is a type variable is not constant,
   a slot through a reference to a reference is. *)
let test_constant_values _ =
  assert_equal (Type ExpressionNotConstant)
    (the_error (typing_constant int32 (A.FunctionCall (q "describe", A.Positional [var "generic"]))));
  let slot = A.PointerSlotAccessor (var "refref", i "value") in
  let typed = TypeCheckExpr.infer (ctx ()) slot in
  match get (typing_constant (TastUtil.get_type NoSpan typed) slot) with
  | TConstant (_, _, _, _, value, _) -> assert_equal typed value
  | _ -> assert_failure "not a constant"

let suite =
  "Unexecuted checker lines" >::: [
      "is_numeric_literal of every expression" >:: test_numeric_literal;
      "literal_matches of every expression" >:: test_literal_matches;
      "annotation_types_literal of every expression" >:: test_annotation_types_literal;
      "signed size widths" >:: test_signed_size_widths;
      "pragma arguments" >:: test_pragma_arguments;
      "an undeclared temporary" >:: test_undeclared_temporary;
      "a name declared twice" >:: test_name_declared_twice;
      "assigning a temporary" >:: test_assign_temporary;
      "checked as inferred" >:: test_checked_as_inferred;
      "a pointer slot of a non-reference" >:: test_pointer_slot_of_non_reference;
      "a method of no instance" >:: test_method_of_no_instance;
      "the order of a block" >:: test_block_order;
      "an assigned value in place" >:: test_assigned_value_in_place;
      "an assignment through a non-reference" >:: test_assign_through_non_reference;
      "a case on a non-union" >:: test_case_on_non_union;
      "a function with a stored body" >:: test_function_with_stored_body;
      "invalid pragmas" >:: test_invalid_pragmas;
      "constant values" >:: test_constant_values
    ]

let _ = run_test_tt_main suite
