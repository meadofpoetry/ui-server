open Containers

let round x   = (if Float.(x < (floor x +. 0.5)) then floor x else ceil x) |> int_of_float
let px        = Printf.sprintf "%dpx"
let translate = Printf.sprintf "translate(%dpx, %dpx)"
let (//) x y  = round @@ (float_of_int x) /. (float_of_int y)
let rec gcd a b =
  if a <> 0 && b <> 0
  then let a, b = if a > b then a mod b, b else a, b mod a in gcd a b
  else a + b
let resolution_to_aspect (w,h) =
  let d = gcd w h in w / d, h / d

let sum_scroll_offsets (e:Dom_html.element Js.t) =
  let rec aux cur acc_left acc_top =
    match Js.Opt.to_option cur with
    | None     -> acc_left,acc_top
    | Some cur ->
       (match Js.to_string cur##.nodeName with
        | "BODY" -> acc_left,acc_top
        | _      ->
           aux cur##.parentNode
             (acc_left + (Js.Unsafe.coerce cur)##.scrollLeft)
             (acc_top  + (Js.Unsafe.coerce cur)##.scrollTop))
  in
  aux e##.parentNode 0 0

module Keyboard_event = struct

  type key_name =
    [ `Enter
    | `Escape
    | `Space
    | `End
    | `Home
    | `Arrow_left
    | `Arrow_right
    | `Delete
    | `Unknown ]

  let event_to_key (e : Dom_html.keyboardEvent Js.t) : key_name =
    let key = Option.map Js.to_string @@ Js.Optdef.to_option e##.key in
    (match key, e##.keyCode with
     | Some "Enter", _ | _, 13 -> `Enter
     | Some "Escape", _ | _, 27 -> `Escape
     | Some "Space", _ | _, 32 -> `Space
     | Some "End", _ | _, 35 -> `End
     | Some "Home", _ | _, 36 -> `Home
     | Some "ArrowLeft", _ | _, 37 -> `Arrow_left
     | Some "ArrowRight", _ | _, 39 -> `Arrow_right
     | Some "Delete", _ | _, 46 -> `Delete
     | _ -> `Unknown)

end

module Animation = struct

  module Timing = struct
    let pi = 4.0 *. atan 1.0
    let in_out_sine x =  0.5 *. (1. -. (cos (pi *. x)))
  end

  let animate ~(timing   : float -> float)
              ~(draw     : float -> unit)
              ~(duration : float) =
    let start = Unix.gettimeofday () in

    let rec cb = (fun _ ->

        let time          = Unix.gettimeofday () in
        let time_fraction = Float.min ((time -. start) /. duration) 1. in
        let progress      = timing time_fraction in
        let ()            = draw progress in

        if Float.(time_fraction < 1.)
        then
          let _ = Dom_html.window##requestAnimationFrame (Js.wrap_callback cb) in
          ())
    in

    let _ = Dom_html.window##requestAnimationFrame (Js.wrap_callback cb) in
    ()

end
