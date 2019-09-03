open Js_of_ocaml
open Js_of_ocaml_tyxml

include module type of Components_tyxml.Item_list
module Markup : sig
  include module type of Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)
end

module Event : sig
  class type detail = object
    method item : Dom_html.element Js.t Js.readonly_prop
    method originalEvent : Dom_html.event Js.t Js.readonly_prop
  end
  val action : detail Js.t Widget.custom_event Js.t Dom_html.Event.typ
end

module Item : sig

  class type t =
    object
      inherit Widget.t

      method secondary_text : string option

      method set_secondary_text : string -> unit

      method text : string option

      method set_text : string -> unit

      method activated : bool

      method selected : bool

      method ripple : Ripple.t option

    end

  val make :
    ?ripple:bool ->
    ?activated:bool ->
    ?selected:bool ->
    ?secondary_text:string ->
    ?graphic:#Widget.t ->
    ?meta:#Widget.t ->
    ?role:string ->
    string -> t

  val attach : ?ripple:bool -> #Dom_html.element Js.t -> t

end

class type t =
  object
    inherit Widget.t

    (** Initialize selectedIndex value based on pre-selected checkbox list
        items, single selection or radio. *)
    method initialize_list_type : unit -> unit

    method wrap_focus : bool

    (** Sets the list to allow the up arrow on the first element to focus
        the last element of the list and vice versa. *)
    method set_wrap_focus : bool -> unit

    method vertical : bool

    (** Sets the list to an orientation causing the keys used for navigation to
        change. [true] results in the Up/Down arrow keys being used.
        [false] results in the Left/Right arrow keys being used. *)
    method set_vertical : bool -> unit

    method use_activated : bool

    (** Sets the selection logic to apply/remove the mdc-list-item--activated
        class. *)
    method set_use_activated : bool -> unit

    method single_selection : bool

    (** Sets the list to be a selection list. Enables the enter and space keys
        for selecting/deselecting a list item. *)
    method set_single_selection : bool -> unit

    method dense : bool

    (** Styles the density of the list. If [true], list appears more compact. *)
    method set_dense : bool -> unit

    method non_interactive : bool

    (** If [true], disables interactivity affordances. *)
    method set_non_interactive : bool -> unit

    method selected_elements : Dom_html.element Js.t list

    method selected_indexes : int list

    method set_selected_item : Item.t -> unit

    method set_selected_items : Item.t list -> unit

    method set_selected_index : int -> unit

    method set_selected_indexes : int list -> unit

    method items : Dom_html.element Js.t list

    (* Private methods *)

    method private items_ : Dom_html.element Dom.nodeList Js.t

    (** Return [true] if it is single selection list, checkbox list or radio list. *)
    method private is_selectable_list : bool

    method private set_selected : Dom_html.element Js.t list -> unit

    method private set_single_selection_ : Dom_html.element Js.t -> unit

    (** Sets aria attribute for single selection to given item. *)
    method private set_aria_for_single_selection : Dom_html.element Js.t -> unit

    method private set_selected_item_on_action :
                     ?toggle:bool ->
                     Dom_html.element Js.t ->
                     unit

    (** Notifies a user that item was selected. *)
    method private notify_action : Dom_html.event Js.t -> Dom_html.element Js.t -> unit

    (** Handles `keydown` event. *)
    method private handle_keydown :
                     Dom_html.keyboardEvent Js.t ->
                     unit Lwt.t -> unit Lwt.t

    (** Handles `click` event. *)
    method private handle_click :
                     Dom_html.mouseEvent Js.t ->
                     unit Lwt.t -> unit Lwt.t

    (** Handles `focusin` event. *)
    method private handle_focus_in :
                     Dom_html.event Js.t ->
                     unit Lwt.t -> unit Lwt.t

    (** Handles `focusout` event *)
    method private handle_focus_out :
                     Dom_html.event Js.t ->
                     unit Lwt.t -> unit Lwt.t

    (** Toggles nested radio of given item.
        Radio doesn't change the checked state if it is already checked. *)
    method private set_radio : Dom_html.element Js.t -> unit

    (** Checks nested checkboxes of given items. Checkboxes of other items
        are unchecked. *)
    method private set_checkbox : Dom_html.element Js.t list -> unit

    (** Toggles nested checkbox of given item. *)
    method private toggle_checkbox :
                     ?toggle:bool ->
                     Dom_html.element Js.t ->
                     unit

  end

val make :
  ?avatar_list:bool ->
  ?dense:bool ->
  ?two_line:bool ->
  ?non_interactive:bool ->
  ?role:string ->
  #Widget.t list -> t

val attach : #Dom_html.element Js.t -> t
