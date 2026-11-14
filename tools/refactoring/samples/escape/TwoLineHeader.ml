(* escapes: stage_two_lines
   clear: guarded_two_lines *)

(* The `=` of the helper's header is not on the line of its `let`. *)
let helper_two_lines
      (n: int): int =
  if n < 0 then raise (Austral_error (negative n)) else n

let stage_two_lines (n: int): int Outcome.t =
  let checked = helper_two_lines n in
  Outcome.catch (fun _ -> checked)

let guarded_two_lines (n: int): int Outcome.t =
  Outcome.catch (fun _ -> helper_two_lines n)
