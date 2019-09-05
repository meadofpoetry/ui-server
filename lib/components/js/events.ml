open Js_of_ocaml
open Js_of_ocaml_lwt

module Typ = struct
  open Dom_events.Typ

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

  let (wheel : wheelEvent Js.t typ) = make "wheel"

  let (focusin : Dom_html.event Js.t typ) = make "focusin"

  let (focusout : Dom_html.event Js.t typ) = make "focusout"
end

let wheel ?use_capture ?passive target : Typ.wheelEvent Js.t Lwt.t =
  Lwt_js_events.make_event Typ.wheel ?use_capture ?passive target

let wheels ?cancel_handler ?use_capture ?passive t =
  Lwt_js_events.seq_loop wheel ?cancel_handler ?use_capture ?passive t

let focusin ?use_capture ?passive target =
  Lwt_js_events.make_event Typ.focusin ?use_capture ?passive target

let focusins ?cancel_handler ?use_capture ?passive t =
  Lwt_js_events.seq_loop focusin ?cancel_handler ?use_capture ?passive t

let focusout ?use_capture ?passive target =
  Lwt_js_events.make_event Typ.focusout ?use_capture ?passive target

let focusouts ?cancel_handler ?use_capture ?passive t =
  Lwt_js_events.seq_loop focusout ?cancel_handler ?use_capture ?passive t
