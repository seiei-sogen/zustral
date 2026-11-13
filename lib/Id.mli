(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** This module contains opaque types for IDs used by the environment.

The reason this is defined outside the `Env` module is to avoid a cyclic
dependency (`Env` depends on `Tast` and `Tast` depends on IDs).

The type of an ID hides its number: nothing outside this module can see it or
choose it. The [fresh_*] functions below make an ID from the next number of a
supply. Any module of the library could call them, with any supply, also
[Supply.initial], and get a number the compilation has already given out. Only
the environment does: it holds the supply of the compilation, and makes an ID
when something is registered in it. No type can say that, so a check does
(tools/refactoring/id-makers.py). *)

(** The type of module IDs. *)
type mod_id
[@@deriving eq]

(** The type of instance method IDs. *)
type ins_meth_id
[@@deriving (eq, show)]

(** The type of monomorph IDs. *)
type mono_id
[@@deriving (eq, show)]

(** {1 IDs that say what kind of declaration they stand for}

    Each of these takes the next number of the declarations, so the kinds
    share one sequence, and prints as the others do. A record's and a union's
    ID take theirs from a sequence of their own ({!Supply.next_type_id}). Its
    type says which kind of declaration the environment made it for, so the
    lookup that takes it returns a declaration of that kind. *)

(** The type of constant IDs. *)
type constant_id

(** The type of union case IDs. *)
type union_case_id

(** The type of typeclass method IDs. *)
type typeclass_method_id

(** The type of function IDs. *)
type function_id
[@@deriving (eq, show)]

(** The type of typeclass IDs. *)
type typeclass_id
[@@deriving (eq, show)]

(** The type of record IDs. *)
type record_id
[@@deriving (eq, show)]

(** The type of union IDs. *)
type union_id
[@@deriving (eq, show)]

(** The type of instance IDs. *)
type instance_id
[@@deriving (eq, show)]

(** The ID of the declaration a named type names: a record or a union. *)
type type_decl_id =
  | RecordTypeId of record_id
  | UnionTypeId of union_id
[@@deriving (eq, show)]

(** {1 Monomorph IDs that say what kind of monomorph they stand for}

    Each of these takes the next number of the monomorphs, so the four kinds
    share one sequence and the names in the generated C do not depend on the
    kind. A type of a monomorph (a record or a union) and the C code name a
    monomorph by its {!mono_id}; the functions below give it. *)

type record_mono_id
[@@deriving eq]

type union_mono_id
[@@deriving eq]

type function_mono_id
[@@deriving eq]

type method_mono_id
[@@deriving eq]

val mono_id_of_record : record_mono_id -> mono_id
val mono_id_of_union : union_mono_id -> mono_id
val mono_id_of_function : function_mono_id -> mono_id
val mono_id_of_method : method_mono_id -> mono_id

(** The next ID of each kind, and the supply to take the one after it from. *)

val fresh_mod_id : Supply.t -> mod_id * Supply.t
val fresh_constant_id : Supply.t -> constant_id * Supply.t
val fresh_union_case_id : Supply.t -> union_case_id * Supply.t
val fresh_typeclass_method_id : Supply.t -> typeclass_method_id * Supply.t
val fresh_function_id : Supply.t -> function_id * Supply.t
val fresh_typeclass_id : Supply.t -> typeclass_id * Supply.t
val fresh_record_id : Supply.t -> record_id * Supply.t
val fresh_union_id : Supply.t -> union_id * Supply.t
val fresh_instance_id : Supply.t -> instance_id * Supply.t
val fresh_ins_meth_id : Supply.t -> ins_meth_id * Supply.t
val fresh_record_mono_id : Supply.t -> record_mono_id * Supply.t
val fresh_union_mono_id : Supply.t -> union_mono_id * Supply.t
val fresh_function_mono_id : Supply.t -> function_mono_id * Supply.t
val fresh_method_mono_id : Supply.t -> method_mono_id * Supply.t

(** The order IDs were made in. A set of IDs keeps its elements in it. *)

val compare_instance_id : instance_id -> instance_id -> int

(** The number of an ID. It is for the name that the generated C gives to what
    the ID identifies, and for nothing else. *)

val function_id_number : function_id -> int
val ins_meth_id_number : ins_meth_id -> int
val mono_id_number : mono_id -> int
