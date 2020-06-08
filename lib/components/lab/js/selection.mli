(* Inspired by https://github.com/Simonwep/selection *)

open Js_of_ocaml

type mode = Touch | Center | Cover

type elt = Query of string | Node of Dom_html.element Js.t

type event = {
  area : Dom_html.element Js.t;
  original_event : Dom_html.event Js.t;
  selected : Dom_html.element Js.t list;
  removed : Dom_html.element Js.t list;
  added : Dom_html.element Js.t list;
}

class type t =
  object
    inherit Components.Widget.t

    method set_single_click : bool -> unit

    method set_multiple : bool -> unit

    method set_mode : mode -> unit

    method set_disabled : bool -> unit

    method selected : Dom_html.element Js.t list

    method remove_from_selection : Dom_html.element Js.t -> unit

    method clear_selection : ?store:bool -> unit -> unit
    (** Clear the elements which were saved by 'keep_selection'. *)

    method resolve_selectables : unit -> unit
    (** Can be used if during a selection elements have been added. Will update
        everything which can be selected. *)

    method keep_selection : unit -> unit
    (** Saves the current selection for the next selection. Allowes multiple
        selections. *)

    method select : Dom_html.element Js.t list -> unit

    method select_query : string list -> unit
  end

val make :
  ?multiple:bool ->
  ?single_click:bool ->
  ?start_threshold:int ->
  ?scroll_speed_divider:float ->
  ?start_areas:elt list ->
  ?boundaries:elt list ->
  ?selectables:elt list ->
  ?mode:mode ->
  ?class_:string ->
  ?container:elt ->
  ?before_start:(t -> event -> bool) ->
  ?on_start:(t -> event -> unit) ->
  ?on_move:(t -> event -> unit) ->
  ?on_stop:(t -> event -> unit) ->
  ?on_outside_click:(t -> event -> unit) ->
  unit ->
  t
