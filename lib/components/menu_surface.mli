open Js_of_ocaml
open Js_of_ocaml_tyxml

include module type of Components_tyxml.Menu_surface

module Markup : sig
  include module type of Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)
end

type position =
  { top : float
  ; right : float
  ; left : float
  ; bottom : float
  }

type point =
  { x : float
  ; y : float
  }

type layout =
  { viewport : int * int
  ; viewport_distance : position
  ; anchor_height : float
  ; anchor_width : float
  ; surface_height : float
  ; surface_width : float
  ; body_dimensions : int * int
  ; window_scroll : int * int
  }

val make_position :
  ?top:float ->
  ?right:float ->
  ?left:float ->
  ?bottom:float ->
  unit -> position

module Corner : sig
  type t =
    | Top_left
    | Top_right
    | Bottom_left
    | Bottom_right
    | Top_start
    | Top_end
    | Bottom_start
    | Bottom_end

  val is_bottom : t -> bool

  val is_right : t -> bool

  val is_flip_rtl : t -> bool
end

module Const : sig
  val transition_open_duration_s : float
  val transition_close_duration_s : float
  val margin_to_edge : float
  val anchor_to_menu_surface_width_ratio : float
end

module Event : sig
  (** Event emitted after the menu surface is opened. *)
  val opened : unit Widget.custom_event Js.t Events.Typ.typ

  (** Event emitted after the menu surface is closed. *)
  val closed : unit Widget.custom_event Js.t Events.Typ.typ
end

class t : Dom_html.element Js.t -> unit -> object
  val mutable _anchor_element : Dom_html.element Js.t option
  val mutable _anchor_corner : Corner.t
  val mutable _anchor_margin : position
  val mutable _quick_open : bool
  val mutable _is_open : bool
  val mutable _previous_focus : Dom_html.element Js.t option
  val mutable _hoisted_element : bool
  val mutable _is_fixed_position : bool

  val mutable _first_focusable : Dom_html.element Js.t option
  val mutable _last_focusable : Dom_html.element Js.t option
  val mutable _animation_thread : unit Lwt.t option

  val mutable _dimensions : int * int
  val mutable _position : point

  (** Event listeners. *)

  val mutable _keydown_listener : unit Lwt.t option
  val mutable _body_click_listener : unit Lwt.t option

  inherit Widget.t

  (** Public API *)

  (** Indicates whether the menu surface is open. *)
  method is_open : bool

  (** Closes the menu. *)
  method close : unit -> unit Lwt.t

  (** Opens the menu surface. *)
  method reveal : unit -> unit Lwt.t

  (** Sets whether the menu surface has been hoisted to the body so that
        the offsets are calculated relative to the page and not the anchor. *)
  method set_is_hoisted : bool -> unit

  (** Removes the menu-surface element from the DOM and appends it to the
        body element. Should be used to overcome [overflow: hidden] issues. *)
  method hoist_menu_to_body : unit -> unit

  (** Sets whether the menu surface should open and close without animation when
        the open/close methods are called. *)
  method set_quick_open : bool -> unit

  (** Sets the element that the menu-surface is anchored to. *)
  method set_anchor_element : #Dom_html.element Js.t -> unit

  (** Sets whether the menu surface is using fixed positioning. *)
  method set_fixed_position : bool -> unit

  (** Sets the absolute x/y position to position based on.
        Requires the menu to be hoisted. *)
  method set_absolute_position : point -> unit

  (** Sets the distance from the anchor point that the menu surface
        should be shown. *)
  method set_anchor_margin : position -> unit

  (** Sets the corner that the menu surface will be anchored to. *)
  method set_anchor_corner : Corner.t -> unit

  (** Private methods *)

  method private handle_open : unit -> unit

  method private handle_close : unit -> unit

  (** Provides logic to open the menu surface. *)
  method private open_ : unit -> unit Lwt.t

  (** Provides logic to close the menu surface. *)
  method private close_ : unit -> unit Lwt.t

  (** Handles clicks and close if not within menu-surface element. *)
  method private handle_body_click :
                   Dom_html.mouseEvent Js.t ->
                   unit Lwt.t -> unit Lwt.t

  (** Handles keydowns. *)
  method private handle_keydown :
                   Dom_html.keyboardEvent Js.t ->
                   unit Lwt.t -> unit Lwt.t

  (** Computes the corner of the anchor from which to animate and position
        the menu surface. *)
  method private get_origin_corner : layout -> Corner.t

  (** The last focused element when the menu surface was opened should
        regain focus, if the user is focused on or within the menu surface
        when it is closed. *)
  method private maybe_restore_focus : unit -> unit

  method private notify_open : unit -> unit

  method private notify_close : unit -> unit

  (** Returns the horizontal offset of menu surface origin corner from
        the corresponding anchor corner. *)
  method private get_horizontal_origin_offset : layout -> Corner.t -> float

  (** Returns the vertical offset of menu surface origin corner from
        the corresponding anchor corner. *)
  method private get_vertical_origin_offset : layout -> Corner.t -> float

  (** Returns the maximum height of the menu surface, based on available space.
        [0] indicates that [max-height] property should not be set. *)
  method private get_menu_surface_max_height : layout -> Corner.t -> float

  method private auto_position : unit -> unit

  (** Calculates the offsets for positioning the menu-surface when
        the menu-surface has been hoisted to the body. *)
  method private adjust_position_for_hoisted_element :
                   layout ->
                   (string * float) list ->
                   (string * float) list

  (** Returns measurements used to position menu surface popup. *)
  method private get_auto_layout_measurements : unit -> layout

  method private set_position : (string * float) list -> unit
end

val make :
  ?fixed:bool ->
  ?open_:bool ->
  Dom_html.element Js.t list -> t

val attach : #Dom_html.element Js.t -> t
