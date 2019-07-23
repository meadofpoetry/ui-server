open Js_of_ocaml
open Js_of_ocaml_tyxml

include module type of Components_tyxml.Menu
module Markup : sig
  include module type of Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)
end

module Event : sig
  include module type of Menu_surface.Event

  class type selected =
    object
      method index : int Js.readonly_prop
      method item : Dom_html.element Js.t Js.readonly_prop
    end

  (** Used to indicate when an element has been selected.
      This event also includes the item selected and the list index
      of that item. *)
  val selected : selected Js.t Widget.custom_event Js.t Dom_html.Event.typ
end

class t : ?body:Dom_html.element Js.t
  -> ?viewport:Menu_surface.viewport
  -> ?list:Item_list.t
  -> ?focus_on_open:bool
  -> Dom_html.element Js.t
  -> unit
  -> object
    inherit Menu_surface.t

    (** Public API. *)

    method wrap_focus : bool

    method set_wrap_focus : bool -> unit

    method list : Item_list.t option

    method items : Dom_html.element Js.t list

    (** Sets the index of the menu item that will be focused every time the menu
        opens. Pass [None] to indicate that the root menu element, rather than
        a specific list item, should receive focus when the menu opens
        (this is the default behavior). *)
    method set_default_focus_item_index : int option -> unit

    (** Private methods. *)

    method private notify_selected : Dom_html.element Js.t -> unit

    method private handle_item_action : Dom_html.element Js.t -> unit Lwt.t

    (** Handles toggling the selected classes in a selection group
        when a selection is made. *)
    method private handle_selection_group :
      item:Dom_html.element Js.t ->
      Dom_html.element Js.t ->
      unit

    (** Returns the parent selection group of an element if one exists. *)
    method private get_selection_group :
      Dom_html.element Js.t ->
      Dom_html.element Js.t Js.opt
  end

val make_of_item_list : ?body:#Dom_html.element Js.t
  -> ?viewport:Menu_surface.viewport
  -> ?focus_on_open:bool
  -> ?fixed:bool
  -> ?open_:bool
  -> Item_list.t
  -> t

val attach : ?body:#Dom_html.element Js.t
  -> ?viewport:Menu_surface.viewport
  -> ?focus_on_open:bool
  -> #Dom_html.element Js.t
  -> t
