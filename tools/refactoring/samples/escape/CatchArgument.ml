(* escapes: stage_thunk
   clear: guarded_thunk *)

(* An argument of Outcome.catch that is not a `fun` is run before catch is
   called: here the helper raises while it makes the function to catch. *)
let helper_thunk (n: int): unit -> int =
  if n < 0 then raise (Austral_error (negative n)) else (fun _ -> n)

let stage_thunk (n: int): int Outcome.t =
  Outcome.catch (helper_thunk n)

let guarded_thunk (n: int): int Outcome.t =
  Outcome.catch (fun _ -> helper_thunk n ())
