open Components_tyxml

module CSS = struct

  let root = "resizable"
  let active = BEM.add_modifier root "active"
  let resizers = BEM.add_element root "resizers"

  let resizer = BEM.add_element root "resizer"
  let resizer_top_left = BEM.add_modifier resizer "top-left"
  let resizer_top_right = BEM.add_modifier resizer "top-right"
  let resizer_bottom_left = BEM.add_modifier resizer "bottom-left"
  let resizer_bottom_right = BEM.add_modifier resizer "bottom-right"
  let resizer_top = BEM.add_modifier resizer "top"
  let resizer_bottom = BEM.add_modifier resizer "bottom"
  let resizer_left = BEM.add_modifier resizer "left"
  let resizer_right = BEM.add_modifier resizer "right"

end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html
  open Utils

  let create ?(tabindex = -1) ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([ a_class classes
            ; a_tabindex tabindex ] <@> attrs)
      [ div ~a:[a_class [CSS.resizers]]
          [ div ~a:[a_class [CSS.resizer; CSS.resizer_top_left]] []
          ; div ~a:[a_class [CSS.resizer; CSS.resizer_top_right]] []
          ; div ~a:[a_class [CSS.resizer; CSS.resizer_bottom_left]] []
          ; div ~a:[a_class [CSS.resizer; CSS.resizer_bottom_right]] []
          ]
      ]

end
