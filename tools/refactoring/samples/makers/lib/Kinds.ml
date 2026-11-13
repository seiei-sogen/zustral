(* flagged: fresh_decl_id fresh_mono_function_id *)

(* A kind added later, a maker named after `open Id` or through an alias, and
   a maker split over two lines. *)
open Id

module I = Id

let next (supply: Supply.t): mono_function_id * Supply.t =
  fresh_mono_function_id supply

let make = I.fresh_decl_id

let other (supply: Supply.t) =
  I.
    fresh_decl_id supply
