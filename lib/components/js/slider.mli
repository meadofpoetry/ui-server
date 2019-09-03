open Js_of_ocaml
open Js_of_ocaml_tyxml

include module type of Components_tyxml.Slider
module Markup : sig
  include module type of Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)
end

type event =
  | Mouse of Dom_html.mouseEvent Js.t
  | Touch of Dom_html.touchEvent Js.t

module Event : sig
  class type event =
    object
      inherit [float] Widget.custom_event
    end
  val input : event Js.t Dom_html.Event.typ
  val change : event Js.t Dom_html.Event.typ
end

class type t =
  object
    inherit Widget.t

    (** Public API *)

    method discrete : bool

    method vertical : bool

    method has_track_marker : bool

    method disabled : bool

    method set_disabled : bool -> unit

    method min : float

    method set_min : float -> unit

    method max : float

    method set_max : float -> unit

    method step : float option

    method set_step : float option -> unit

    method value : float

    method set_value : float -> unit

    method step_up : ?amount:float -> unit -> unit

    method step_down : ?amount:float -> unit -> unit

    (** Private methods *)

    method private set_active_ : bool -> unit

    method private notify_input : unit -> unit

    method private set_value_ : ?force:bool -> fire_input:bool -> float -> unit

    method private percent_to_value : float -> float

    method private round_to_step : step:float -> float -> float

    method private get_offset : Dom_html.clientRect Js.t -> float * float

    method private get_mouse_position : event -> float * float

    method private calculate_thumb_styles : float -> unit

    method private calculate_track_styles : #Dom_html.element Js.t -> float -> unit

    method private remove_track_markers : unit -> unit

    method private append_track_markers : int -> unit

    method private set_marker_value : float -> unit

    method private setup_track_marker : unit -> unit

    method private calculate_percent : event -> float

    method private reduce_value : float -> float option

    method private handle_focus : Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t

    method private handle_blur : Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t

    method private handle_mouse_enter :
                     Dom_html.mouseEvent Js.t ->
                     unit Lwt.t -> unit Lwt.t

    method private handle_mouse_leave :
                     Dom_html.mouseEvent Js.t ->
                     unit Lwt.t -> unit Lwt.t

    method private handle_touch_start :
                     Dom_html.touchEvent Js.t ->
                     unit Lwt.t -> unit Lwt.t

    method private handle_mouse_down :
                     Dom_html.mouseEvent Js.t ->
                     unit Lwt.t -> unit Lwt.t

    method private handle_touch_end :
                     Dom_html.touchEvent Js.t ->
                     unit Lwt.t -> unit Lwt.t

    method private handle_mouse_up :
                     Dom_html.mouseEvent Js.t ->
                     unit Lwt.t -> unit Lwt.t

    method private handle_touch_move :
                     Dom_html.touchEvent Js.t ->
                     unit Lwt.t -> unit Lwt.t

    method private handle_mouse_move :
                     Dom_html.mouseEvent Js.t ->
                     unit Lwt.t -> unit Lwt.t

    method private handle_keydown :
                     Dom_html.keyboardEvent Js.t ->
                     unit Lwt.t -> unit Lwt.t

    method private handle_move : ?fire_input:bool -> event -> unit Lwt.t

    method private handle_drag_end : unit -> unit
  end

val make :
  ?classes:string list ->
  ?discrete:bool ->
  ?markers:bool ->
  ?disabled:bool ->
  ?label:string ->
  ?step:float ->
  ?min:float ->
  ?max:float ->
  ?value:float ->
  unit -> t

val attach : #Dom_html.element Js.t -> t
