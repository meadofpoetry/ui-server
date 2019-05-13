open Js_of_ocaml
open Containers

let prevent_scroll = ref false

type timer_id = Dom_html.timeout_id_safe

let set_timeout (f : unit -> unit) (t : float) : timer_id =
  Dom_html.setTimeout f t

let clear_timeout (timer : timer_id) =
  Dom_html.clearTimeout timer

type interval_id = Dom_html.interval_id

let set_interval (f : unit -> unit) (t : float) : interval_id =
  let cb = Js.wrap_callback f in
  Dom_html.window##setInterval cb t

let clear_interval (interval : interval_id) : unit =
  Dom_html.window##clearInterval interval

let is_in_viewport ?(vertical = true) ?(horizontal = true)
      (e : Dom_html.element Js.t) : bool =
  let height =
    Js.Optdef.get Dom_html.window##.innerHeight
      (fun () -> Dom_html.document##.documentElement##.clientHeight) in
  let width =
    Js.Optdef.get Dom_html.window##.innerWidth
      (fun () -> Dom_html.document##.documentElement##.clientWidth) in
  let rect = e##getBoundingClientRect in
  let vertical =
    not vertical
    || (rect##.top >. 0. && rect##.bottom <=. (float_of_int height)) in
  let horizontal =
    not horizontal
    || (rect##.left >. 0. && rect##.right <=. (float_of_int width)) in
  vertical && horizontal

(** Tail-recursive append that does not raise stack overflow on lagre lists *)
let append (a : 'a list) (b : 'a list) : 'a list =
  List.rev_append (List.rev a) b

let ( @ ) = append

let time (name : string) : unit =
  Js.Unsafe.global##.console##time (Js.string name)

let time_end (name : string) : unit =
  Js.Unsafe.global##.console##timeEnd (Js.string name)

let round (x : float) : int =
  (if Float.(x < (floor x +. 0.5)) then floor x else ceil x)
  |> int_of_float

let px : int -> string = Printf.sprintf "%dpx"

let px_js (v : int) : Js.js_string Js.t =
  Js.string @@ px v

let translate = Printf.sprintf "translate(%dpx, %dpx)"

let ( // ) x y =
  round @@ (float_of_int x) /. (float_of_int y)

let rec gcd a b =
  if a <> 0 && b <> 0
  then
    let a, b =
      if a > b then a mod b, b
      else a, b mod a in gcd a b
  else a + b

let resolution_to_aspect (w,h) =
  let d = gcd w h in w / d, h / d

let sum_scroll_offsets (e : Dom_html.element Js.t) =
  let rec aux cur acc_left acc_top =
    match Js.Opt.to_option cur with
    | None -> acc_left, acc_top
    | Some cur ->
       begin match Js.to_string cur##.nodeName with
       | "BODY" -> acc_left,acc_top
       | _ ->
          aux cur##.parentNode
            (acc_left + (Js.Unsafe.coerce cur)##.scrollLeft)
            (acc_top + (Js.Unsafe.coerce cur)##.scrollTop)
       end
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
    | `Unknown
    ]

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

  type frame_id = Dom_html.animation_frame_request_id

  let request_animation_frame (f : float -> unit) : frame_id =
    Dom_html.window##requestAnimationFrame (Js.wrap_callback f)

  let cancel_animation_frame (id : frame_id) : unit =
    Dom_html.window##cancelAnimationFrame id

end
