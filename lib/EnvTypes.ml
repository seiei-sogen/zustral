(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Types used in the various environments. *)
open Id
open InstanceIdSet
open Identifier
open Common
open Type
open TypeParameter
open TypeParameters
open Stages.Tast
open MonoType
open MonoTypeBindings
open Stages.Mtast

(** A module record contains a module's name and the instances it imports. *)
type mod_rec = ModRec of {
      id: mod_id;
      name: module_name;
      imported_instances: InstanceIdSet.t;
    }

type mod_input = {
    name: module_name;
    imported_instances: InstanceIdSet.t;
  }

(** A record, as the environment holds it. *)
type record_decl = {
    id: record_id;
    mod_id: mod_id;
    vis: type_vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    universe: universe;
    slots: typed_slot list;
  }

(** A union, as the environment holds it. Its cases are declarations of their
    own. *)
type union_decl = {
    id: union_id;
    mod_id: mod_id;
    vis: type_vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    universe: universe;
  }

(** A case of a union, as the environment holds it. *)
type union_case_decl = {
    id: union_case_id;
    mod_id: mod_id;
    vis: vis;
    (** This is {!VisPublic} if the parent union is public, {!VisPrivate} otherwise. *)
    union_id: union_id;
    name: identifier;
    docstring: docstring;
    slots: typed_slot list;
  }

(** What a function has besides its signature. *)
type function_impl =
  | BodyNotAttached
  (** A function written in Austral, until its body is stored after type
      checking ({!Env.store_function_body}). *)
  | Body of tstmt
  | Foreign of string * ty ForeignType.foreign_signature
  (** A foreign function: the name of the underlying function it calls, and
      its signature, checked when it was registered. *)
  | ForeignGeneric
  (** A foreign function with type parameters. Its signature is not checked:
      the type checker rejects it ({!DeclarationErrors.ForeignTypeParameters}),
      and that error stays the one reported. *)

(** A function, as the environment holds it. *)
type function_decl = {
    id: function_id;
    mod_id: mod_id;
    vis: vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    value_params: value_parameter list;
    rt: ty;
    impl: function_impl;
  }

(** A typeclass, as the environment holds it. *)
type typeclass_decl = {
    id: typeclass_id;
    mod_id: mod_id;
    vis: vis;
    name: identifier;
    docstring: docstring;
    param: type_parameter;
  }

(** An instance of a typeclass, as the environment holds it. Its methods are
    not declarations: see {!ins_meth_rec}. *)
type instance_decl = {
    id: instance_id;
    mod_id: mod_id;
    vis: vis;
    typeclass_id: typeclass_id;
    docstring: docstring;
    typarams: typarams;
    argument: ty;
  }

(** A method of a typeclass, as the environment holds it. *)
type typeclass_method_decl = {
    id: typeclass_method_id;
    mod_id: mod_id;
    vis: vis;
    (** This is the visibility of the parent typeclass. *)
    typeclass_id: typeclass_id;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    value_params: value_parameter list;
    rt: ty;
  }

(** A declaration record represents a declaration.

   The definition of declaration here is slightly different than that of the
   spec: we include union cases and typeclass methods as "declarations". The
   reason being that we want union cases and typeclass methods to share a
   namespace with real declarations, and this is an easy way to make that
   happen.

*)
type decl =
  | Constant of {
      id: constant_id;
      mod_id: mod_id;
      vis: vis;
      name: identifier;
      ty: ty;
      docstring: docstring;
    }
  | Record of record_decl
  | Union of union_decl
  | UnionCase of union_case_decl
  | Function of function_decl
  | TypeClass of typeclass_decl
  | TypeClassMethod of typeclass_method_decl
  | Instance of instance_decl

(** Represents an instance method. *)
type ins_meth_rec = InsMethRec of {
      id: ins_meth_id;
      instance_id: instance_id;
      method_id: typeclass_method_id;
      docstring: docstring;
      name: identifier;
      typarams: typarams;
      value_params: value_parameter list;
      rt: ty;
      body: tstmt option;
    }

(** Callable things. *)
type callable =
  | FunctionCallable of function_id * typarams * value_parameter list * ty
  | RecordConstructor of record_id * typarams * universe * typed_slot list
  | UnionConstructor of {
      union_id: union_id;
      type_params: typarams;
      universe: universe;
      case: typed_case;
    }
  | MethodCallable of {
      typeclass_id: typeclass_id;
      value_parameters: value_parameter list;
      return_type: ty
    }

(** Represents a monomorph. *)
type monomorph =
  | MonoRecordDefinition of {
      id: record_mono_id;
      type_id: record_id;
      tyargs: mono_type_bindings;
      slots: (mono_slot list) option;
      (** The list of slots, if instantiated. *)
    }
  | MonoUnionDefinition of {
      id: union_mono_id;
      type_id: union_id;
      tyargs: mono_type_bindings;
      cases: (mono_case list) option;
      (** The list of cases, if instantiated. *)
    }
  | MonoFunction of {
      id: function_mono_id;
      function_id: function_id;
      tyargs: mono_type_bindings;
      body: mstmt option;
      (** The function body, if instantiated. *)
    }
  | MonoInstanceMethod of {
      id: method_mono_id;
      method_id: ins_meth_id;
      tyargs: mono_type_bindings;
      body: mstmt option;
    }

type const_input = {
    mod_id: mod_id;
    vis: vis;
    name: identifier;
    ty: ty;
    docstring: docstring;
  }

type record_input = {
    mod_id: mod_id;
    vis: type_vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    universe: universe;
    slots: typed_slot list;
  }

type union_input = {
    mod_id: mod_id;
    vis: type_vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    universe: universe;
  }

type union_case_input = {
    mod_id: mod_id;
    vis: vis;
    union_id: union_id;
    name: identifier;
    docstring: docstring;
    slots: typed_slot list;
  }

type function_input = {
    mod_id: mod_id;
    vis: vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    value_params: value_parameter list;
    rt: ty;
    impl: function_impl;
  }

type type_class_input = {
    mod_id: mod_id;
    vis: vis;
    name: identifier;
    docstring: docstring;
    param: type_parameter;
  }

type type_class_method_input = {
    mod_id: mod_id;
    vis: vis;
    typeclass_id: typeclass_id;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    value_params: value_parameter list;
    rt: ty;
  }

type instance_input = {
    mod_id: mod_id;
    vis: vis;
    typeclass_id: typeclass_id;
    docstring: docstring;
    typarams: typarams;
    argument: ty;
  }

type instance_method_input = {
    instance_id: instance_id;
    method_id: typeclass_method_id;
    docstring: docstring;
    name: identifier;
    typarams: typarams;
    value_params: value_parameter list;
    rt: ty;
    body: tstmt option;
  }

(** Something to register in the environment: a declaration, or a method of an
    instance. The parameter is the type of the ID that registering it returns. *)
type _ registration =
  | NewConstant : const_input -> constant_id registration
  | NewRecord : record_input -> record_id registration
  | NewUnion : union_input -> union_id registration
  | NewUnionCase : union_case_input -> union_case_id registration
  | NewFunction : function_input -> function_id registration
  | NewTypeClass : type_class_input -> typeclass_id registration
  | NewTypeClassMethod : type_class_method_input -> typeclass_method_id registration
  | NewInstance : instance_input -> instance_id registration
  | NewInstanceMethod : instance_method_input -> ins_meth_id registration
