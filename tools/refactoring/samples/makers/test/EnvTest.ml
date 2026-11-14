(* flagged: Supply.initial *)

(* A unit test goes through `Env.empty_env`, not a supply of its own. *)
let test_supply () =
  let supply = Supply . initial in
  ignore supply
