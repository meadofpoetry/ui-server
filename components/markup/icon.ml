open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class     = "mdc-icon"
  let button_class   = CSS.add_modifier base_class "button"
  let disabled_class = CSS.add_modifier base_class "disabled"

  module Font = struct
    let create ?(classes=[]) ?attrs ~icon () =
      Html.i ~a:([a_class ("material-icons" :: base_class :: classes)] <@> attrs) [pcdata icon]
  end
end
