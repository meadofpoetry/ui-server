open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class      = "mdc-ripple-surface"
  let primary_class   = CSS.add_modifier base_class "primary"
  let accent_class    = CSS.add_modifier base_class "accent"
  let unbounded_attr  = a_user_data "mdc-ripple-is-unbounded" ""

end
