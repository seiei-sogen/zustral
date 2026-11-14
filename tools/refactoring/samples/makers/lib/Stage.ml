(* flagged: Supply.initial fresh_function_id *)

(* A stage that makes an ID from a supply of its own: the number is one the
   environment has already given out. *)
let shadow_id (): function_id =
  let (id, _) = Id.fresh_function_id Supply.initial in
  id
