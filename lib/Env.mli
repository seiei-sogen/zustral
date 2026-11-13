(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Type
open MonoType
open MonoTypeBindings
open Stages.Tast
open Stages.Mtast
open Id
open LexEnv
open EnvTypes

(** {1 Types} *)

(** The global environment type. *)
type env

(** {1 Constants} *)

(** The empty compiler. *)
val empty_env : env

(** {1 The supply of numbers}

    The environment holds the supply of the compilation. The IDs it makes when
    something is registered come from there. *)

(** Run a stage that takes numbers and registers nothing. The stage gets the
    supply, and returns its result with the supply it has not used. The
    environment that comes back holds that supply.

    The stage often gets [env] as well, and that [env] still holds the supply
    the stage was given. So a stage must not call [add_module], [register] or
    an [add_or_get_*_monomorph]. Each takes the next number from that same
    supply, which the stage takes too, so one number is given out twice. And
    the environment it returns is dropped here, with everything else the stage
    changed in [env] ([add_exported_function], the [store_*] functions). *)
val with_supply : env -> (Supply.t -> ('a * Supply.t) Outcome.t) -> (env * 'a) Outcome.t

(** {1 The two kinds of lookup}

    A lookup by a name returns an option. The name comes from the program being
    compiled, and the caller knows which error to report when nothing has it.

    A lookup by an ID returns what the ID stands for. This module makes an ID
    when something is registered, so the thing exists. An ID given to an
    environment older than the one that made it is a bug in the compiler: it
    raises an internal error here, and callers do not handle it.

    A monomorph has an ID, and also a key: what it is a monomorph of, and the
    type arguments. A lookup by the key returns an option, because a key can
    name a monomorph that no pass has registered. *)

(** {1 Module Functions} *)

(** Add a module's information to the environment. *)
val add_module : env -> mod_input -> (env * mod_id)

(** Retrieve a module by its name, returning {!None} if it doesn't exist. *)
val get_module_by_name : env -> module_name -> mod_rec option

(** Return a module's name from its ID. *)
val module_name_from_id : Error.place -> env -> mod_id -> module_name

(** {1 Declaration Functions} *)

(** {2 Create} *)

(** Register a declaration or an instance method. Returns the environment that
    has it, and its ID. This is the only way either enters the environment.

    Raises a declaration error if a constant, record, union, function or
    typeclass takes a name that a declaration of its module already has. Union
    cases, typeclass methods, instances and instance methods are not checked. *)
val register : Error.place -> env -> 'id registration -> (env * 'id)

(** Raise the error [register] raises when a declaration of the module already
    has the name. For a check that must report a taken name before its own
    errors, as the extraction of a foreign function does. *)
val name_must_be_free : Error.place -> env -> mod_id -> identifier -> unit

(** Given a function ID, the export name of that function and its checked
    signature, register them in the environment. *)
val add_exported_function : env -> function_id -> string -> ty ForeignType.foreign_signature -> env

(** {2 Retrieve} *)

(** Retrieve a function by its ID. *)
val get_function : env -> function_id -> function_decl

(** Retrieve a typeclass by its ID. *)
val get_typeclass : Error.place -> env -> typeclass_id -> typeclass_decl

(** Retrieve a record by its ID. *)
val get_record : env -> record_id -> record_decl

(** Retrieve a union by its ID. *)
val get_union : Error.place -> env -> union_id -> union_decl

(** Retrieve an instance by its ID. *)
val get_instance_decl : Error.place -> env -> instance_id -> instance_decl

(** Retrieve a declaration by its sourced name, returning {!None} if it doesn't exist.

    If a module with the given name doesn't exist, raises an error. *)
val get_decl_by_name : Error.place -> env -> sident -> decl option

(** Find a typeclass method from its typeclass ID and name, returning {!None} if
    the typeclass has no method of that name. *)
val get_method_from_typeclass_id_and_name : env -> typeclass_id -> identifier -> typeclass_method_decl option

(** Return all typeclass instances defined in a module. *)
val module_instances: env -> mod_id -> instance_decl list

(** Return the public typeclass instances defined in a module. *)
val module_public_instances: env -> mod_id -> instance_decl list

(** Return the list of cases of a union. *)
val get_union_cases : env -> union_id -> union_case_decl list

(** Get method from the instance ID and name, returning {!None} if the instance
    does not define a method of that name. *)
val get_instance_method_from_instance_id_and_method_name : env -> instance_id -> identifier -> ins_meth_rec option

(** Get an instance method by ID. *)
val get_instance_method : env -> ins_meth_id -> ins_meth_rec

(** Return a list of all exported functions in the environment, as (ID, export
    name, checked signature) triples. *)
val get_export_functions : env -> (function_id * string * ty ForeignType.foreign_signature) list

(** {2 Update} *)

(** Store the given function body in the function with the given ID, returning
    the new environment. *)
val store_function_body : env -> function_id -> tstmt -> env

(** Store the given instance method body in the instance method with the given
    ID, returning the new environment. *)
val store_method_body : env -> ins_meth_id -> tstmt -> env

(** {1 Callable Functions} *)

(** Get a callable given its name and the name of the importing module. *)
val get_callable : Error.place -> env -> module_name -> sident -> callable option

(** {1 Monomorph Functions} *)

(** Given a record's ID and a list of arguments, register a monomorph and return
   its ID, or return the ID if it exists. *)
val add_or_get_record_monomorph : env -> record_id -> mono_type_bindings -> (env * record_mono_id)

(** Given a union's ID and a list of arguments, register a monomorph and return
   its ID, or return the ID if it exists. *)
val add_or_get_union_monomorph : env -> union_id -> mono_type_bindings -> (env * union_mono_id)

(** Given a function's ID and a list of type arguments, register a monomorph and
   return its ID, or return the ID if it exists. *)
val add_or_get_function_monomorph : env -> function_id -> mono_type_bindings -> (env * function_mono_id)

(** Given an instance method's ID and a list of type arguments, register a
   monomorph and return its ID, or return the ID if it exists. *)
val add_or_get_instance_method_monomorph : env -> ins_meth_id -> mono_type_bindings -> (env * method_mono_id)

val store_record_monomorph_definition : env -> record_mono_id -> mono_slot list -> env

val store_union_monomorph_definition : env -> union_mono_id -> mono_case list -> env

val store_function_monomorph_definition : env -> function_mono_id -> mstmt -> env

val store_instance_method_monomorph_definition : env -> method_mono_id -> mstmt -> env

(** Given the ID of a record and a set of monomorphic type arguments, return
   the ID of the corresponding monomorph, or {!None} if no pass has registered
   it. *)
val get_record_monomorph : env -> record_id -> mono_type_bindings -> record_mono_id option

(** {!get_record_monomorph} for a union. *)
val get_union_monomorph : env -> union_id -> mono_type_bindings -> union_mono_id option

(** Get all uninstantiated monomorphs. *)
val get_uninstantiated_monomorphs : env -> monomorph list

(** Get a monomorph by ID. *)
val get_monomorph : env -> mono_id -> monomorph

(** Given the ID of a record's or a union's monomorph, return the module that
    declares the type, the type's signature, and the monomorph's type
    arguments. *)
val get_type_monomorph : env -> mono_id -> (mod_id * TypeSignature.type_signature * mono_type_bindings)

(** Get the type of a variable, trying first the lexenv and then the env for
    constants. *)
val get_variable : Error.place -> env -> lexenv -> qident -> (ty * var_source) option

(** Return all typeclass instances visible from a module. These are not only the
    instances that are defined in the module itself, but the instances imported
    by the module. *)
val visible_instances : Error.place -> env -> mod_id -> instance_decl list
