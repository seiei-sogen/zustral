(* missed: stage_operator, stage_closure, stage_reexport *)

(* The limits of the check, as the top of escape-check.py lists them. Each
   stage below can raise, and the check clears it. *)

(* An operator is not a name the check follows. *)
let ( +! ) (a: int) (b: int): int =
  if a < 0 then raise (Austral_error (negative a)) else a + b

let stage_operator (n: int): int Outcome.t =
  Ok (n +! 1)

(* A function made in one place and called in another: the call names a
   variable, not the function. *)
let stage_closure (n: int): int Outcome.t =
  match Outcome.catch (fun _ -> (fun m -> if m < 0 then raise (Austral_error (negative m)) else m)) with
  | Ok check -> Ok (check n)
  | Error errors -> Error errors

(* A function reached through the `include` of another module: this file
   does not mention the module that defines it. *)
let stage_reexport (n: int): int Outcome.t =
  Ok (Reexport.helper n)
