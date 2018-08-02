open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class          = "mdc-typography"
  let headline1_class     = CSS.add_modifier base_class "headline1"
  let headline2_class     = CSS.add_modifier base_class "headline2"
  let headline3_class     = CSS.add_modifier base_class "headline3"
  let headline4_class     = CSS.add_modifier base_class "headline4"
  let headline5_class     = CSS.add_modifier base_class "headline5"
  let headline6_class     = CSS.add_modifier base_class "headline6"
  let subtitle1_class     = CSS.add_modifier base_class "subtitle1"
  let subtitle2_class     = CSS.add_modifier base_class "subtitle2"
  let body1_class         = CSS.add_modifier base_class "body1"
  let body2_class         = CSS.add_modifier base_class "body2"
  let button_class        = CSS.add_modifier base_class "button"
  let caption_class       = CSS.add_modifier base_class "caption"
  let overline_class      = CSS.add_modifier base_class "overline"
  let adjust_margin_class = CSS.add_modifier base_class "adjust-margin"

end
