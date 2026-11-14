(* missed: *)

(* The supply a compilation starts with, written without `Supply.` in front.
   An ID made from it needs a `fresh_<kind>_id`, which is seen. *)
open Supply

let restart (): t = initial

let again (): Supply.t = Supply.(initial)
