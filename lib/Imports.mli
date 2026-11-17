(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Id

(** Represents the set of all imports into a module. *)
type import_map

(** Create an empty map, given the name of the module that's we're importing
    into and whether that module is safe or unsafe. *)
val empty_map : module_name -> Common.module_kind -> import_map

(** Add a qualified identifier to the import map. *)
val add_symbol : import_map -> qident -> import_map

(** Add the ID of a typeclass instance into the import map. *)
val add_instance_to_imports : import_map -> instance_id -> import_map

(** The name of the module we're importing into. *)
val importing_module : import_map -> module_name

(** Whether the module we're importing into is safe or unsafe. What a safe
    module may not write (`@embed`) is rejected where its names are
    qualified, and that is done with the import map. *)
val importing_module_kind : import_map -> Common.module_kind

(** Find a symbol from its local nickname. Returns {!None} if not imported. *)
val get_symbol : import_map -> identifier -> qident option

(** The imported symbols, each under its local nickname. *)
val imported_symbols : import_map -> qident list

(** The list of imported typeclass instances. *)
val imported_instances : import_map -> instance_id list
