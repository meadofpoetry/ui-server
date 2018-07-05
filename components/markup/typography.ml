open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class          = "mdc-typography"
  let display4_class      = CSS.add_modifier base_class "display4"
  let display3_class      = CSS.add_modifier base_class "display3"
  let display2_class      = CSS.add_modifier base_class "display2"
  let display1_class      = CSS.add_modifier base_class "display1"
  let headline_class      = CSS.add_modifier base_class "headline"
  let title_class         = CSS.add_modifier base_class "title"
  let subheading2_class   = CSS.add_modifier base_class "subheading2"
  let subheading1_class   = CSS.add_modifier base_class "subheading1"
  let body2_class         = CSS.add_modifier base_class "body2"
  let body1_class         = CSS.add_modifier base_class "body1"
  let caption_class       = CSS.add_modifier base_class "caption"
  let button_class        = CSS.add_modifier base_class "button"
  let adjust_margin_class = CSS.add_modifier base_class "adjust-margin"

end
