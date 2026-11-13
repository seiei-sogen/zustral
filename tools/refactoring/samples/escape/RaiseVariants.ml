(* escapes: stage_notrace, stage_backtrace
   clear: guarded_notrace *)

(* The two other ways the standard library raises. *)
let helper_notrace (n: int): int =
  if n < 0 then raise_notrace (Austral_error (negative n)) else n

let helper_backtrace (error: exn) (trace: Printexc.raw_backtrace): int =
  Printexc.raise_with_backtrace error trace

let stage_notrace (n: int): int Outcome.t =
  let checked = helper_notrace n in
  Outcome.catch (fun _ -> checked)

let stage_backtrace (error: exn) (trace: Printexc.raw_backtrace): int Outcome.t =
  Ok (helper_backtrace error trace)

let guarded_notrace (n: int): int Outcome.t =
  Outcome.catch (fun _ -> helper_notrace n)
