open Js_of_ocaml
open Js_of_ocaml_tyxml

include module type of Components_tyxml.Radio
module Markup : sig
  include module type of Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)
end

class type t =
  object
    inherit Widget.t

    method value : string

    method set_value : string -> unit

    method disabled : bool

    method set_disabled : bool -> unit

    method checked : bool

    method toggle : ?notify:bool -> ?force:bool -> unit -> unit

    method input_element : Dom_html.inputElement Js.t

    method ripple : Ripple.t option

    (** Private methods. *)

    method private notify_change : unit -> unit

    method private create_ripple : unit -> Ripple.t
  end

val make :
  ?input_id:string ->
  ?name:string ->
  ?checked:bool ->
  ?disabled:bool ->
  ?on_change:(bool -> unit) ->
  unit -> t

val attach : ?on_change:(bool -> unit) -> #Dom_html.element Js.t -> t
