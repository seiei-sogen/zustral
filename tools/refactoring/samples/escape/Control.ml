(* escapes: stage
   clear: guarded *)

(* The shape the check has always seen: a helper at the top level, its header
   on one line, a plain raise. *)
let helper (n: int): int =
  if n < 0 then raise (Austral_error (negative n)) else n

let stage (n: int): int Outcome.t =
  let checked = helper n in
  Outcome.catch (fun _ -> checked)

let guarded (n: int): int Outcome.t =
  Outcome.catch (fun _ -> helper n)
