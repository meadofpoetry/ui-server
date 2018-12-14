type t =
  { v : float
  ; steps : int
  ; step_duration : float
  ; exn : exn option
  }

exception Timeout of t

let steps ~step_duration seconds =
  seconds *. (1. /. step_duration)
  |> int_of_float

let create ?exn ~step_duration seconds =
  { v = seconds
  ; steps = steps ~step_duration seconds
  ; step_duration
  ; exn
  }

let period (t : t) = t.v

let reset (t : t) =
  { t with steps = steps ~step_duration:t.step_duration t.v }

let step (t : t) = match pred t.steps with
  | x when x < 0 ->
     begin match t.exn with
     | None -> raise_notrace (Timeout t)
     | Some exn -> raise_notrace exn
     end
  | steps -> { t with steps }
