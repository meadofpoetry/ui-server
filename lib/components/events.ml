open Js_of_ocaml
open Js_of_ocaml_lwt

class type event = Dom_html.event

module Typ = struct

  include Dom_events.Typ

  type 'a t = 'a typ

  type delta_mode =
    | DOM_DELTA_PIXEL
    | DOM_DELTA_LINE
    | DOM_DELTA_PAGE

  class type wheelEvent =
    object
      inherit Dom_html.mouseEvent
      method deltaX : int Js.readonly_prop
      method deltaY : int Js.readonly_prop
      method deltaZ : int Js.readonly_prop
      method deltaMode : delta_mode Js.readonly_prop
    end

  (** https://www.w3.org/TR/pointerevents/#dom-pointerevent *)
  class type pointerEvent =
    object
      inherit Dom_html.mouseEvent
      method pointerId : int Js.readonly_prop
      method width : float Js.readonly_prop
      method height : float Js.readonly_prop
      method pressure : float Js.readonly_prop
      method tangentialPressure : float Js.readonly_prop
      method tiltX : int Js.readonly_prop
      method tiltY : int Js.readonly_prop
      method twist : int Js.readonly_prop
      method pointerType : Js.js_string Js.t Js.readonly_prop
      method isPrimary : bool Js.t Js.readonly_prop
    end

  let (wheel : wheelEvent Js.t typ) =
    make "wheel"

  let (pointerdown : pointerEvent Js.t typ) =
    make "pointerdown"

  let (focusin : Dom_html.event Js.t typ) =
    make "focusin"

  let (focusout : Dom_html.event Js.t typ) =
    make "focusout"

end

module Key = struct

  type t =
    [ `Tab
    | `Enter
    | `Escape
    | `Space
    | `End
    | `Home
    | `Arrow_left
    | `Arrow_right
    | `Arrow_up
    | `Arrow_down
    | `Delete
    | `Page_up
    | `Page_down
    | `Numpad_multiply
    | `Char of char
    | `Digit of int (* integers in range 0 - 9 *)
    | `Unknown
    ]

  let of_event (e : #Dom_html.event Js.t) : t =
    let key = match Js.Optdef.to_option (Js.Unsafe.coerce e)##.key with
      | None -> None
      | Some x -> Some (Js.to_string x) in
    let key_code = Js.Optdef.to_option (Js.Unsafe.coerce e)##.keyCode in
    (match key, key_code with
     | Some "Tab", _ | _, Some 9 -> `Tab
     | Some "Enter", _ | _, Some 13 -> `Enter
     | Some "Escape", _ | _, Some 27 -> `Escape
     | Some "Space", _ | _, Some 32 -> `Space
     | Some "PageUp", _ | _, Some 33 -> `Page_up
     | Some "PageDown", _ | _, Some 34 -> `Page_down
     | Some "End", _ | _, Some 35 -> `End
     | Some "Home", _ | _, Some 36 -> `Home
     | Some "ArrowLeft", _ | _, Some 37 -> `Arrow_left
     | Some "ArrowUp", _ | _, Some 38 -> `Arrow_up
     | Some "ArrowRight", _ | _, Some 39 -> `Arrow_right
     | Some "ArrowDown", _ | _, Some 40 -> `Arrow_down
     | Some "Delete", _ | _, Some 46 -> `Delete
     | Some "*", _ | _, Some 106 -> `Numpad_multiply
     | _, Some x when x >= 48 && x <= 57 ->
        let d = int_of_string @@ Char.escaped @@ Char.chr x in
        `Digit d
     | _, Some x when x >= 65 && x <= 90 ->
        let char = Char.chr x in
        let char =
          if Js.to_bool (Js.Unsafe.coerce e)##.shiftKey
          then Char.uppercase_ascii char
          else char in
        `Char char
     | _ -> `Unknown)

end

include (Dom_events : module type of Dom_events with module Typ := Typ)
include Lwt_js_events

let listen_lwt :
      'a. ?cancel_handler:bool -> ?use_capture:bool ->
      #Dom.node Js.t -> (#event as 'a) Js.t Typ.t ->
      ('a Js.t -> unit Lwt.t -> unit Lwt.t) ->
      unit Lwt.t =
  fun ?cancel_handler ?use_capture node x f ->
  seq_loop (make_event x)  ?cancel_handler ?use_capture node f

let wheel ?use_capture target =
  make_event Typ.wheel ?use_capture target

let wheels ?cancel_handler ?use_capture t =
  seq_loop wheel ?cancel_handler ?use_capture t

let pointerdown ?use_capture target =
  make_event Typ.pointerdown ?use_capture target

let pointerdowns ?cancel_handler ?use_capture t =
  seq_loop pointerdown ?cancel_handler ?use_capture t

let focusin ?use_capture target =
  make_event Typ.focusin ?use_capture target

let focusins ?cancel_handler ?use_capture t =
  seq_loop focusin ?cancel_handler ?use_capture t

let focusout ?use_capture target =
  make_event Typ.focusout ?use_capture target

let focusouts ?cancel_handler ?use_capture t =
  seq_loop focusout ?cancel_handler ?use_capture t
