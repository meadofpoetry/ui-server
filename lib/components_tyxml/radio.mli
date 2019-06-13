module CSS : sig
  val root : string
  val native_control : string
  val background : string
  val outer_circle : string
  val inner_circle : string
  val disabled : string
end

module Make :
functor(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
          with module Svg := Svg) ->
sig
  val create :
    ?classes:string list ->
    ?attrs:Html_types.div_attrib Html.attrib list ->
    ?input_id:string ->
    ?checked:bool ->
    ?disabled:bool ->
    ?name:string ->
    unit -> [> Html_types.div ] Html.elt
end
