val string_of_float : float -> string

module CSS : sig
  val root : string

  val container : string

  val track : string

  val track_marker_container : string

  val track_marker : string

  val thumb_container : string

  val thumb : string

  val focus_ring : string

  val pin : string

  val pin_value_marker : string

  val track_before : string

  val track_after : string

  val vertical : string

  val active : string

  val disabled : string

  val discrete : string

  val focus : string

  val display_markers : string
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml with module Svg := Svg) : sig
  val create :
       ?classes:string list
    -> ?attrs:Html_types.div_attrib Html.attrib list
    -> ?discrete:bool
    -> ?markers:bool
    -> ?disabled:bool
    -> ?label:string
    -> ?step:float
    -> ?min:float
    -> ?max:float
    -> ?value:float
    -> unit
    -> [> Html_types.div] Html.elt
end
