(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open IdentifierMap
open Id
open Error

type import_map =
  ImportMap of {
      import_into: module_name;
      import_into_kind: Common.module_kind;
      symbols: qident IdentifierMap.t;
      instances: instance_id list;
    }

let empty_map (n: module_name) (kind: Common.module_kind): import_map =
  ImportMap {
      import_into = n;
      import_into_kind = kind;
      symbols = IdentifierMap.empty;
      instances = [];
    }

let get_symbol (im: import_map) (name: identifier): qident option =
  let (ImportMap { symbols; _ }) = im in
  IdentifierMap.find_opt name symbols

let add_symbol (im: import_map) (q: qident) =
  match get_symbol im (local_name q) with
  | Some _ ->
     generic_error NoSpan SymbolImportedTwice
  | None ->
     let (ImportMap fields) = im in
     ImportMap { fields with symbols = IdentifierMap.add (local_name q) q fields.symbols }

let add_instance_to_imports (im: import_map) (id: instance_id): import_map =
  let (ImportMap fields) = im in
  ImportMap { fields with instances = id :: fields.instances }

let importing_module (im: import_map): module_name =
  let (ImportMap { import_into; _ }) = im in
  import_into

let importing_module_kind (im: import_map): Common.module_kind =
  let (ImportMap { import_into_kind; _ }) = im in
  import_into_kind

let imported_instances (im: import_map): instance_id list =
  let (ImportMap { instances; _ }) = im in
  instances
