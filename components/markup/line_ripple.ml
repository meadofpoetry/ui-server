open Utils
open Containers

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-line-ripple"
  let active_class = CSS.add_modifier base_class "active"
  let deactivating_class = CSS.add_modifier base_class "deactivating"

  let create ?(classes = []) ?attrs () : 'a elt =
    div ~a:([a_class (base_class :: classes)] <@> attrs) []

end
