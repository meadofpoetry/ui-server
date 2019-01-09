open Js_of_ocaml

type anchor = [`Left | `Right | `Top | `Bottom]

class type t =
  object
    inherit Widget.t

    method anchor : anchor
    method set_anchor : anchor -> unit
    method drawer : Widget.t
    method show : unit -> unit
    method show_await : unit -> unit Lwt.t
    method hide : unit -> unit
    method toggle : unit -> unit
    method state : bool
    method s_state : bool React.signal

    (* Private methods *)

    method private _disable_scroll : unit -> unit
    method private _enable_scroll : unit -> unit

    method private get_delta : x:int -> y:int ->
                               touch:Dom_html.touch Js.t -> int
    method private on_click : Dom_html.mouseEvent Js.t -> unit Lwt.t
    method private on_keydown : Dom_html.keyboardEvent Js.t -> bool
    method private on_event_end : Dom_html.touchEvent Js.t -> unit
    method private on_touchstart : Dom_html.touchEvent Js.t -> unit Lwt.t
    method private on_touchmove : Dom_html.touchEvent Js.t -> unit Lwt.t
  end

val make : ?anchor:anchor -> ?animating:bool -> content:#Widget.t list ->
           unit -> t

val attach : ?anchor:anchor -> ?animating:bool -> #Dom_html.element Js.t -> t
