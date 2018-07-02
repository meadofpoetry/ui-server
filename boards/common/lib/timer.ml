let to_period x step_duration = x * int_of_float (1. /. step_duration)

type t = { v : int; count : int; step : float }

exception Timeout of t

let create ~step s =
  { v = s; count = to_period s step; step }
let period (t:t)   = t.v

let reset (t:t)    = { t with count = to_period t.v t.step }

let step (t:t)     = match pred t.count with
  | x when x < 0 -> raise_notrace (Timeout t)
  | count        -> { t with count }
