(* clear: *)

(* The environment holds the supply of the compilation: it may make IDs. *)
let empty_env: env = Env { supply = Supply.initial }

let add_function (env: env): env * function_id =
  let (id, supply) = Id.fresh_function_id (env_supply env) in
  (with_supply env supply, id)
