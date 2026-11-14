(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** Compiler stages. *)

(** The AST is the CST, but imports are qualified and `let` statements are
    reshaped. *)
module Ast = struct
  open Identifier
  open Common
  open Span
  open Escape

  type qtypespec =
    | QTypeSpecifier of qident * qtypespec list
    | QReadRef of qtypespec * qtypespec
    | QWriteRef of qtypespec * qtypespec
    | QSpan of qtypespec * qtypespec
    | QSpanWrite of qtypespec * qtypespec

  type qbinding =
    QBinding of {
        name: identifier;
        ty: qtypespec;
        rename: identifier;
      }

  type astmt =
    | ASkip of span
    | ALet of span * mutability * identifier * qtypespec * astmt
    | ADestructure of span * mutability * qbinding list * aexpr * astmt
    | AAssign of span * lvalue * aexpr
    | AInitialAssign of qident * qtypespec * aexpr
    | AIf of span * aexpr * astmt * astmt
    | AWhen of span * aexpr * astmt
    | ACase of span * aexpr * abstract_when list
    | AWhile of span * aexpr * astmt
    | AFor of {
        span: span;
        name: identifier;
        initial: aexpr;
        final: aexpr;
        body: astmt
      }
    | ABorrow of {
        span: span;
        original: identifier;
        rename: identifier;
        region: identifier;
        body: astmt;
        mode: borrow_stmt_kind
      }
    | ABlock of span * astmt * astmt
    | ADiscarding of span * aexpr
    | AReturn of span * aexpr

  and aexpr =
    | NilConstant
    | BoolConstant of bool
    | IntConstant of NumberLiteral.int_literal
    | FloatConstant of NumberLiteral.float_literal
    | StringConstant of escaped_string
    | Variable of qident
    | FunctionCall of qident * abstract_arglist
    | ArithmeticExpression of arithmetic_operator * aexpr * aexpr
    | Comparison of comparison_operator * aexpr * aexpr
    | Conjunction of aexpr * aexpr
    | Disjunction of aexpr * aexpr
    | Negation of aexpr
    | IfExpression of aexpr * aexpr * aexpr
    | Path of qident * path_elem list
    | RefPath of qident * path_elem list
    | Embed of qtypespec * string * aexpr list
    | Deref of aexpr
    | Typecast of aexpr * qtypespec
    | SizeOf of qtypespec
    | BorrowExpr of borrowing_mode * qident
    | Reborrow of qident

  and abstract_when =
    | AbstractWhen of identifier * qbinding list * astmt

  and abstract_arglist =
    | Positional of aexpr list
    | Named of (identifier * aexpr) list

  and path_elem =
    | SlotAccessor of identifier
    | PointerSlotAccessor of identifier
    | ArrayIndex of aexpr

  (** The left side of an assignment: a variable and a path into it. *)
  and lvalue = LValue of qident * path_elem list
end

(** The combined representation merges the interface and body. *)
module Combined = struct
  open Identifier
  open Common
  open Imports
  open Ast
  open Type
  open TypeParameter
  open TypeParameters
  open Span

  type combined_module = CombinedModule of {
        name: module_name;
        kind: module_kind;
        interface_docstring: docstring;
        interface_imports: import_map;
        body_docstring: docstring;
        body_imports: import_map;
        decls: combined_definition list;
      }

  and combined_definition =
    | CConstant of span * vis * identifier * qtypespec * aexpr * docstring
    | CRecord of span * type_vis * identifier * typarams * universe * qslot list * docstring
    | CUnion of span * type_vis * identifier * typarams * universe * qcase list * docstring
    | CFunction of span * vis * identifier * typarams * qparam list * qtypespec * astmt * docstring * pragma list
    | CTypeclass of span * vis * identifier * type_parameter * combined_method_decl list * docstring
    | CInstance of span * span * vis * qident * typarams * qtypespec * combined_method_def list * docstring
    (** The first span is the definition's. The second is the declaration's
        whose place decides when the instance is registered: the interface's,
        for an instance the interface declares; the definition's, otherwise. *)

  and qslot = QualifiedSlot of identifier * qtypespec

  and qcase = QualifiedCase of identifier * qslot list

  and qparam = QualifiedParameter of identifier * qtypespec

  and combined_method_decl = CMethodDecl of identifier * typarams * qparam list * qtypespec * docstring

  and combined_method_def = CMethodDef of identifier * typarams * qparam list * qtypespec * docstring * astmt
end

(* The AST, but expressions in control structures are lifted into temporaries. *)
module AstLC = struct
  open Identifier
  open Common
  open Span
  open Escape

  type qtypespec = Ast.qtypespec

  type qbinding = Ast.qbinding

  type astmt =
    | ASkip of span
    | ALet of span * mutability * identifier * qtypespec * astmt
    | ADestructure of span * mutability * qbinding list * aexpr * astmt
    | AAssign of span * lvalue * aexpr
    | AInitialAssign of qident * qtypespec * aexpr
    | AIf of span * aexpr * astmt * astmt
    | AWhen of span * aexpr * astmt
    | ACase of span * aexpr * abstract_when list
    | AWhile of span * aexpr * astmt
    | AFor of {
        span: span;
        name: identifier;
        initial: aexpr;
        final: aexpr;
        body: astmt
      }
    | ABorrow of {
        span: span;
        original: identifier;
        rename: identifier;
        region: identifier;
        body: astmt;
        mode: borrow_stmt_kind
      }
    | ABlock of span * astmt * astmt
    | ADiscarding of span * aexpr
    | AReturn of span * aexpr
    | LetTmp of identifier * aexpr
    | AssignTmp of identifier * aexpr

  and aexpr =
    | NilConstant
    | BoolConstant of bool
    | IntConstant of NumberLiteral.int_literal
    | FloatConstant of NumberLiteral.float_literal
    | StringConstant of escaped_string
    | Variable of qident
    | Temporary of identifier
    | FunctionCall of qident * abstract_arglist
    | ArithmeticExpression of arithmetic_operator * aexpr * aexpr
    | Comparison of comparison_operator * aexpr * aexpr
    | Conjunction of aexpr * aexpr
    | Disjunction of aexpr * aexpr
    | Negation of aexpr
    | IfExpression of aexpr * aexpr * aexpr
    | Path of qident * path_elem list
    | RefPath of qident * path_elem list
    | Embed of qtypespec * string * aexpr list
    | Deref of aexpr
    | Typecast of aexpr * qtypespec
    | SizeOf of qtypespec
    | BorrowExpr of borrowing_mode * qident
    | Reborrow of qident

  and abstract_when =
    | AbstractWhen of identifier * qbinding list * astmt

  and abstract_arglist =
    | Positional of aexpr list
    | Named of (identifier * aexpr) list

  and path_elem =
    | SlotAccessor of identifier
    | PointerSlotAccessor of identifier
    | ArrayIndex of aexpr

  (** The left side of an assignment: a variable and a path into it. *)
  and lvalue = LValue of qident * path_elem list
end

(** The AstLC, but paths are desugared. *)
module AstDP = struct
  open Identifier
  open Common
  open Span
  open Escape

  type qtypespec = Ast.qtypespec

  type qbinding = Ast.qbinding

  type astmt =
    | ASkip of span
    | ALet of span * mutability * identifier * qtypespec * astmt
    | ADestructure of span * mutability * qbinding list * aexpr * astmt
    | AAssign of span * aexpr * aexpr
    | AAssignVar of span * qident * aexpr
    | AInitialAssign of qident * qtypespec * aexpr
    | AIf of span * aexpr * astmt * astmt
    | AWhen of span * aexpr * astmt
    | ACase of span * aexpr * abstract_when list
    | AWhile of span * aexpr * astmt
    | AFor of {
        span: span;
        name: identifier;
        initial: aexpr;
        final: aexpr;
        body: astmt
      }
    | ABorrow of {
        span: span;
        original: identifier;
        rename: identifier;
        region: identifier;
        body: astmt;
        mode: borrow_stmt_kind
      }
    | ABlock of span * astmt * astmt
    | ADiscarding of span * aexpr
    | AReturn of span * aexpr
    | LetTmp of identifier * aexpr
    | AssignTmp of identifier * aexpr

  and aexpr =
    | NilConstant
    | BoolConstant of bool
    | IntConstant of NumberLiteral.int_literal
    | FloatConstant of NumberLiteral.float_literal
    | StringConstant of escaped_string
    | Variable of qident
    | Temporary of identifier
    | FunctionCall of qident * abstract_arglist
    | ArithmeticExpression of arithmetic_operator * aexpr * aexpr
    | Comparison of comparison_operator * aexpr * aexpr
    | Conjunction of aexpr * aexpr
    | Disjunction of aexpr * aexpr
    | Negation of aexpr
    | IfExpression of aexpr * aexpr * aexpr
    | Embed of qtypespec * string * aexpr list
    | Deref of aexpr
    | Typecast of aexpr * qtypespec
    | SizeOf of qtypespec
    | BorrowExpr of borrowing_mode * qident
    | Reborrow of qident
    (* Path expressions *)
    | SlotAccessor of aexpr * identifier
    | PointerSlotAccessor of aexpr * identifier
    | ArrayIndex of aexpr * aexpr

  and abstract_when =
    | AbstractWhen of identifier * qbinding list * astmt

  and abstract_arglist =
    | Positional of aexpr list
    | Named of (identifier * aexpr) list
end

(** The AstDP, but anonymous borrows are desugared. *)
module AstDB = struct
  open Identifier
  open Common
  open Span
  open Escape

  type qtypespec = Ast.qtypespec

  type qbinding = Ast.qbinding

  type astmt =
    | ASkip of span
    | ALet of span * mutability * identifier * qtypespec * astmt
    | ADestructure of span * mutability * qbinding list * aexpr * astmt
    | AAssign of span * aexpr * aexpr
    | AAssignVar of span * qident * aexpr
    | AInitialAssign of qident * qtypespec * aexpr
    | AIf of span * aexpr * astmt * astmt
    | AWhen of span * aexpr * astmt
    | ACase of span * aexpr * abstract_when list
    | AWhile of span * aexpr * astmt
    | AFor of {
        span: span;
        name: identifier;
        initial: aexpr;
        final: aexpr;
        body: astmt
      }
    | ABorrow of {
        span: span;
        original: identifier;
        rename: identifier;
        region: identifier;
        body: astmt;
        mode: borrow_stmt_kind
      }
    | ABlock of span * astmt * astmt
    | ADiscarding of span * aexpr
    | AReturn of span * aexpr
    | LetTmp of identifier * aexpr
    | AssignTmp of identifier * aexpr

  and aexpr =
    | NilConstant
    | BoolConstant of bool
    | IntConstant of NumberLiteral.int_literal
    | FloatConstant of NumberLiteral.float_literal
    | StringConstant of escaped_string
    | Variable of qident
    | Temporary of identifier
    | FunctionCall of qident * abstract_arglist
    | ArithmeticExpression of arithmetic_operator * aexpr * aexpr
    | Comparison of comparison_operator * aexpr * aexpr
    | Conjunction of aexpr * aexpr
    | Disjunction of aexpr * aexpr
    | Negation of aexpr
    | IfExpression of aexpr * aexpr * aexpr
    | Embed of qtypespec * string * aexpr list
    | Deref of aexpr
    | Typecast of aexpr * qtypespec
    | SizeOf of qtypespec
    (* Path expressions *)
    | SlotAccessor of aexpr * identifier
    | PointerSlotAccessor of aexpr * identifier
    | ArrayIndex of aexpr * aexpr

  and abstract_when =
    | AbstractWhen of identifier * qbinding list * astmt

  and abstract_arglist =
    | Positional of aexpr list
    | Named of (identifier * aexpr) list
end

(** The combined representation, but bodies are thoroughly desugared. *)
module SmallCombined = struct
  open Identifier
  open Common
  open Imports
  open AstDB
  open Type
  open TypeParameter
  open TypeParameters
  open Span

  type qslot = Combined.qslot
  type qcase = Combined.qcase
  type qparam = Combined.qparam

  type combined_module = CombinedModule of {
        name: module_name;
        kind: module_kind;
        interface_docstring: docstring;
        interface_imports: import_map;
        body_docstring: docstring;
        body_imports: import_map;
        decls: combined_definition list;
      }

  and combined_definition =
    | CConstant of span * vis * identifier * qtypespec * aexpr * docstring
    | CRecord of span * type_vis * identifier * typarams * universe * qslot
 list * docstring
    | CUnion of span * type_vis * identifier * typarams * universe * qcase list * docstring
    | CFunction of span * vis * identifier * typarams * qparam list * qtypespec * astmt * docstring * pragma list
    | CTypeclass of span * vis * identifier * type_parameter * Combined.combined_method_decl list * docstring
    | CInstance of span * span * vis * qident * typarams * qtypespec * combined_method_def list * docstring
    (** The first span is the definition's. The second is the declaration's
        whose place decides when the instance is registered: the interface's,
        for an instance the interface declares; the definition's, otherwise. *)

  and combined_method_def = CMethodDef of identifier * typarams * qparam list * qtypespec * docstring * astmt
end

(** The linked representation is essentially the same as the combined
    representation, but declarations are linked to their corresponding entry in
    the environment. *)
module Linked = struct
  open Identifier
  open Common
  open Imports
  open AstDB
  open Type
  open TypeParameter
  open TypeParameters
  open Id

  type linked_module = LinkedModule of {
        mod_id: mod_id;
        name: module_name;
        kind: module_kind;
        interface_docstring: docstring;
        interface_imports: import_map;
        body_docstring: docstring;
        body_imports: import_map;
        decls: linked_definition list;
      }

  and linked_definition =
    | LConstant of constant_id * vis * identifier * ty * aexpr * docstring
    | LRecord of record_id * type_vis * identifier * typarams * universe * typed_slot list * docstring
    | LUnion of union_id * type_vis * identifier * typarams * universe * linked_case list * docstring
    | LFunction of function_id * vis * identifier * typarams * value_parameter list * ty * astmt * docstring * pragma list
    | LTypeclass of typeclass_id * vis * identifier * type_parameter * linked_method_decl list * docstring
    | LInstance of instance_id * vis * qident * typarams * ty * linked_method_def list * docstring

  and linked_case = LCase of union_case_id * identifier * typed_slot list

  and linked_method_decl = LMethodDecl of typeclass_method_id * identifier * value_parameter list * ty * docstring

  and linked_method_def = LMethodDef of ins_meth_id * identifier * value_parameter list * ty * docstring * astmt
end

(** The typed AST. *)
module Tast = struct
  open Identifier
  open Common
  open Escape
  open Type
  open TypeParameter
  open TypeParameters
  open TypeBindings
  open Span
  open Linked
  open Id

  type typed_binding =
    TypedBinding of {
        name: identifier;
        ty: ty;
        rename: identifier;
      }

  type case_ref =
    | CasePlain
    | CaseRef

  type tstmt =
    | TSkip of span
    | TLet of span * mutability * identifier * ty * tstmt
    | TDestructure of span * mutability * typed_binding list * texpr * tstmt
    | TAssign of span * texpr * texpr
    | TAssignVar of span * qident * texpr
    | TInitialAssign of qident * texpr
    | TIf of span * texpr * tstmt * tstmt
    | TCase of span * texpr * union_id * ty list * typed_when list * case_ref
    (** The ID and the type arguments are those of the union the value, or the
        reference, has as its type. *)
    | TWhile of span * texpr * tstmt
    | TFor of span * identifier * texpr * texpr * tstmt
    | TBorrow of {
        span: span;
        original: identifier;
        rename: identifier;
        region: identifier;
        orig_type: ty;
        ref_type: ty;
        body: tstmt;
        mode: borrow_stmt_kind
      }
    | TBlock of span * tstmt * tstmt
    | TDiscarding of span * texpr
    | TReturn of span * texpr
    | TLetTmp of identifier * ty * texpr
    | TAssignTmp of identifier * texpr

  and texpr =
    | TNilConstant
    | TBoolConstant of bool
    | TIntConstant of NumberLiteral.int_literal
    | TFloatConstant of NumberLiteral.float_literal
    | TStringConstant of escaped_string
    | TConstVar of qident * ty
    | TParamVar of identifier * ty
    | TLocalVar of identifier * ty
    | TFunVar of function_id * ty * type_bindings
    | TTemporary of identifier * ty
    (** Represents accessing a function as a value. *)
    | TFuncall of function_id * qident * texpr list * ty * type_bindings
    | TMethodCall of ins_meth_id * qident * typarams * texpr list * ty * type_bindings
    | TVarMethodCall of {
        source_module_name: module_name;
        typeclass_id: typeclass_id;
        params: value_parameter list;
        method_name: qident;
        args: texpr list;
        dispatch_ty: ty;
        rt: ty;
        bindings: type_bindings;
      }
    | TFptrCall of identifier * texpr list * ty
    | TCast of texpr * ty
    (** A type annotation that changed the expression: it gave a literal its
        type, or it turned a write reference into a read reference. The
        annotation is one the user wrote (`e : T`, or the type of a `let`,
        which annotates the value), or the `Index` that `LiftControlPass`
        puts around the bounds of a `for`. *)
    | TImplicitCast of texpr * ty
    (** A cast the type checker adds: around an argument whose parameter has
        an integer type, and around the operands of an arithmetic expression,
        to the type of the operation. *)
    | TComparison of comparison_operator * texpr * texpr
    | TConjunction of texpr * texpr
    | TDisjunction of texpr * texpr
    | TNegation of texpr
    | TIfExpression of texpr * texpr * texpr
    | TRecordConstructor of ty * (identifier * texpr) list
    | TUnionConstructor of {
        union_id: union_id;
        type_name: qident;
        tyargs: ty list;
        universe: universe;
        case_name: identifier;
        args: (identifier * texpr) list;
      }
    (** The type is `NamedType (type_name, UnionTypeId union_id, tyargs, universe)`: `get_type`
        builds it, so the monomorph of the union and the type of the value
        come from the same type arguments. *)
    | TSlotAccessor of texpr * identifier * ty
    | TPointerSlotAccessor of texpr * identifier * ty
    | TArrayIndex of texpr * texpr * ty * ref_region * borrowing_mode
    | TSpanIndex of texpr * texpr * ty * ref_region * borrowing_mode
    (** The element type, and the region and the mode of the reference to the
        element that the expression is: `get_type` builds that reference type. *)
    | TEmbed of ty * string * texpr list
    | TDeref of texpr * ty
    (** The type is the one of the value the reference points to. *)
    | TSizeOf of ty

  and typed_when =
    TypedWhen of identifier * typed_binding list * tstmt

  and typed_arglist =
    | TPositionalArglist of texpr list
    | TNamedArglist of (identifier * texpr) list

  type typed_method_decl =
    TypedMethodDecl of typeclass_method_id * identifier * value_parameter list * ty

  type typed_method_def =
    TypedMethodDef of ins_meth_id * identifier * value_parameter list * ty * tstmt

  type typed_decl =
    | TConstant of constant_id * vis * identifier * ty * texpr * docstring
    | TRecord of record_id * type_vis * identifier * typarams * universe * typed_slot list * docstring
    | TUnion of union_id * type_vis * identifier * typarams * universe * linked_case list * docstring
    | TFunction of function_id * vis * identifier * typarams * value_parameter list * ty * tstmt * docstring
    | TForeignFunction of function_id * vis * identifier * value_parameter list * ty * string * ty ForeignType.foreign_signature * docstring
    | TTypeClass of typeclass_id * vis * identifier * type_parameter * typed_method_decl list * docstring
    | TInstance of instance_id * vis * qident * typarams * ty * typed_method_def list * docstring

  type typed_module = TypedModule of module_name * typed_decl list
end

(** Monomorphic typed AST. *)
module Mtast = struct
  open Id
  open Identifier
  open Common
  open Escape
  open MonoType

  type mono_binding =
    MonoBinding of {
        name: identifier;
        ty: mono_ty;
        rename: identifier;
      }

  type mono_module = MonoModule of module_name * mdecl list

  and mdecl =
    | MConstant of constant_id * identifier * mono_ty * mexpr
    | MRecord of record_id * identifier * mono_slot list
    | MRecordMonomorph of record_mono_id * mono_slot list
    | MUnion of union_id * identifier * mono_case list
    | MUnionMonomorph of union_mono_id * mono_case list
    | MFunction of function_id * identifier * mvalue_parameter list * mono_ty * mstmt
    | MFunctionMonomorph of function_mono_id * function_id * mvalue_parameter list * mono_ty * mstmt
    | MForeignFunction of function_id * identifier * mvalue_parameter list * mono_ty * mono_ty ForeignType.foreign_signature * string
    | MConcreteInstance of instance_id * qident * mono_ty  * concrete_method list
    | MMethodMonomorph of method_mono_id * mvalue_parameter list * mono_ty * mstmt

  and concrete_method =
    MConcreteMethod of ins_meth_id * identifier * mvalue_parameter list * mono_ty * mstmt

  and mstmt =
    | MSkip
    | MLet of identifier * mono_ty * mstmt
    | MDestructure of mono_binding list * mexpr * mstmt
    | MAssign of mexpr * mexpr
    | MAssignVar of qident * mexpr
    | MInitialAssign of qident * mexpr
    | MIf of mexpr * mstmt * mstmt
    | MCase of mexpr * union_mono_id * mtyped_when list * Tast.case_ref
    | MWhile of mexpr * mstmt
    | MFor of identifier * mexpr * mexpr * mstmt
    | MBorrow of {
        original: identifier;
        rename: identifier;
        region: identifier;
        orig_type: mono_ty;
        ref_type: mono_ty;
        body: mstmt;
        mode: borrow_stmt_kind
      }
    | MBlock of mstmt * mstmt
    | MDiscarding of mexpr
    | MReturn of mexpr
    | MLetTmp of identifier * mono_ty * mexpr
    | MAssignTmp of identifier * mexpr

  and mexpr =
    | MNilConstant
    | MBoolConstant of bool
    | MIntConstant of NumberLiteral.int_literal
    | MFloatConstant of NumberLiteral.float_literal
    | MStringConstant of escaped_string
    | MConstVar of qident * mono_ty
    | MParamVar of identifier * mono_ty
    | MLocalVar of identifier * mono_ty
    | MTemporary of identifier * mono_ty
    | MGenericFunVar of function_mono_id * mono_ty
    | MConcreteFunVar of function_id * mono_ty
    | MConcreteFuncall of function_id * qident * mexpr list * mono_ty
    (** Represents a call to a concrete function. *)
    | MGenericFuncall of function_mono_id * mexpr list * mono_ty
    (** Represents a call to a generic function. *)
    | MConcreteMethodCall of ins_meth_id * qident * mexpr list * mono_ty
    (** Represents a call to an instance method of a concrete instance. *)
    | MGenericMethodCall of ins_meth_id * method_mono_id * mexpr list * mono_ty
    (** Represents a call to an instance method of a generic instance. *)
    | MFptrCall of identifier * mexpr list * mono_ty
    | MCast of mexpr * mono_ty
    | MComparison of comparison_operator * mexpr * mexpr
    | MConjunction of mexpr * mexpr
    | MDisjunction of mexpr * mexpr
    | MNegation of mexpr
    | MIfExpression of mexpr * mexpr * mexpr
    | MRecordConstructor of mono_ty * (identifier * mexpr) list
    | MUnionConstructor of union_mono_id * identifier * (identifier * mexpr) list
    (** The type is the monomorph's: `get_type` builds it. *)
    | MEmbed of mono_ty * string * mexpr list
    | MDeref of mexpr * mono_ty
    (** The type is the one of the value the reference points to. *)
    | MSizeOf of mono_ty
    | MSlotAccessor of mexpr * identifier * mono_ty
    | MPointerSlotAccessor of mexpr * identifier * mono_ty
    | MArrayIndex of mexpr * mexpr * mono_ty * mono_ty * borrowing_mode
    | MSpanIndex of mexpr * mexpr * mono_ty * mono_ty * borrowing_mode
    (** The element type, and the region and the mode of the reference to the
        element that the expression is: `get_type` builds that reference type. *)

  and mtyped_when =
    MTypedWhen of identifier * mono_binding list * mstmt

  and mvalue_parameter =
    MValueParameter of identifier * mono_ty

end
