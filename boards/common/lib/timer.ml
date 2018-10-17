type t =
  { v : float
  ; steps : int
  ; step_duration : float }

exception Timeout of t

let steps ~step_duration seconds =
  seconds *. (1. /. step_duration)
  |> int_of_float

let create ~step_duration seconds =
  { v = seconds
  ; steps = steps ~step_duration seconds
  ; step_duration }

let period (t : t) = t.v

let reset (t : t) =
  { t with steps = steps ~step_duration:t.step_duration t.v }

let step (t : t) = match pred t.steps with
  | x when x < 0 -> raise_notrace (Timeout t)
  | steps        -> { t with steps }
