open Js_of_ocaml

module Typ : sig
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

  val wheel : wheelEvent Js_of_ocaml.Js.t Js_of_ocaml.Dom_events.Typ.typ

  val focusin :
    Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Js_of_ocaml.Dom_events.Typ.typ

  val focusout :
    Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Js_of_ocaml.Dom_events.Typ.typ
end

val wheel :
     ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Typ.wheelEvent Js.t Lwt.t

val wheels :
     ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Typ.wheelEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val focusin :
     ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val focusins :
     ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val focusout :
     ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val focusouts :
     ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t
