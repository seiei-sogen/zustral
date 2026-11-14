(* clear: *)

(* Names that look like makers and are not, and makers that are not code:
   (* Id.fresh_decl_id Supply.initial *) in a nested comment. *)
let rename (supply: Supply.t): identifier * Supply.t =
  let (fresh_id, supply) = Supply.next_borrow_rename supply in
  (make_ident (string_of_int fresh_id), supply)

let temporary (supply: Supply.t) = Identifier.fresh_ident supply

let message: string = "do not call Id.fresh_function_id Supply.initial"

let quote: char = '"'

let refresh_mod_id_cache = ()
