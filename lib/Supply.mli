(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** The numbers the compiler gives to the things it makes.

    A supply holds the next number of each kind. Taking a number returns it
    with the supply to take the following one from, and changes nothing. A
    compilation starts from {!initial} and has one supply, which the
    environment carries from stage to stage. So the numbers of a program do
    not depend on what the process compiled before.

    The numbers are in the names of the generated C and in some messages.
    Here they are plain integers: the module that owns a kind of thing wraps
    its numbers (an ID can only be made by [Id] from a supply). *)
type t

(** The supply a compilation starts with. *)
val initial : t

(** The next number for a module ID, and the supply after it. The functions
    below are the same for the other kinds. *)
val next_mod_id : t -> int * t

val next_decl_id : t -> int * t

val next_ins_meth_id : t -> int * t

val next_mono_id : t -> int * t

(** For the temporary that holds the value of an expression lifted out of a
    statement ([_t<n>]). *)
val next_temporary : t -> int * t

(** For the reference an anonymous borrow is given as a name
    ([<variable>_tmp_ref<n>]). *)
val next_borrow_rename : t -> int * t

(** For the region of an anonymous borrow ([_tmp_region_<n>]). *)
val next_borrow_region : t -> int * t

(** For the region of a borrow statement. The numbers start at 1, because 0 is
    the static region. *)
val next_region : t -> int * t

(** For a variable that only the generated C has ([tmp<n>]). *)
val next_c_variable : t -> int * t

(** Apply a function that takes numbers to the elements of a list, from the
    first to the last, handing the supply on. *)
val map : (t -> 'a -> 'b * t) -> t -> 'a list -> 'b list * t
