(* escapes: stage_in_module, stage_same_name, stage_packed, stage_local_module
   clear: guarded_in_module, stage_harmless *)

(* The helpers are definitions of a module inside the file, so they are
   indented. The local `let limit` before the raise is not one of them: taken
   for one, it would cut the raise off the helper. *)
module Limits = struct
  let helper_in_module (n: int): int =
    let limit = 0 in
    if n < limit then raise (Austral_error (negative n)) else n

  let same_name (n: int): int =
    raise (Austral_error (negative n))
end

(* A second definition of a name must not hide the first. *)
module Quiet = struct
  let same_name (n: int): int = n

  let harmless (n: int): int =
    let next = n + 1 in
    next
end

let stage_in_module (n: int): int Outcome.t =
  let checked = Limits.helper_in_module n in
  Outcome.catch (fun _ -> checked)

let stage_same_name (n: int): int Outcome.t =
  Ok (Limits.same_name n)

let stage_harmless (n: int): int Outcome.t =
  Ok (Quiet.harmless n)

let guarded_in_module (n: int): int Outcome.t =
  Outcome.catch (fun _ -> Limits.helper_in_module n)

(* A module inside a function is part of the function: its definitions must
   not take the text that follows them. *)
let stage_packed (n: int): int Outcome.t =
  register (module struct let accepts (m: int): bool = m >= 0 end : Acceptor);
  if n < 0 then raise (Austral_error (negative n)) else Ok n

let stage_local_module (n: int): int Outcome.t =
  let module Local = struct
    let first (m: int): int = m
    let second (m: int): int = m + 1
  end in
  if n < 0 then raise (Austral_error (negative n)) else Ok (Local.second n)
