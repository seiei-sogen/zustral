(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** The rules that hold for number literals and for no other expression.

    A literal does not say which integer or float type it has. It takes the
    type of the place it stands in. The rules are applied in three stages of
    the compiler, and each of them calls the function of its rule here. *)

open Type
open Stages.Tast

(** {1 Before type checking} *)

(** An arithmetic expression whose operands are integer literals is the
    integer literal of its value. The operands can be such expressions
    themselves. Any other expression is returned as it is. *)
val fold_integer_arithmetic : Stages.Ast.aexpr -> Stages.Ast.aexpr

(** {1 Operands} *)

(** Whether the expression is an integer literal or a float literal. An
    operand of a comparison or of an arithmetic expression that is the only
    literal of the two takes the type of the other operand. *)
val is_numeric_literal : Stages.AstDB.aexpr -> bool

(** {1 A literal where a type is expected} *)

(** Whether the rules for literals accept the value where the type is
    expected, whatever the type of the value itself:

    - an integer literal where an integer type is expected. It has to fit the
      type: if it does not, this raises the error that says so
    - a float literal where a float type is expected

    A literal under a cast is a literal here, whether the cast is an annotation
    or one the type checker added. [false] means that the rules say nothing
    about the value, and its type has to match the expected one. *)
val literal_matches : ty -> texpr -> bool

(** Whether a type annotation gives the value its type: an integer literal
    annotated with an integer type, which it has to fit as above, and a float
    literal annotated with a float type. A literal under a cast is not a
    literal here. *)
val annotation_types_literal : ty -> texpr -> bool
