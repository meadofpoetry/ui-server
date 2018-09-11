open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct

  open Html

  let base_class = "mdc-icon-button"
  let icon_class = CSS.add_element base_class "icon"
  let on_class = CSS.add_modifier base_class "on"
  let icon_on_class = CSS.add_modifier icon_class "on"

  let create ?(classes = []) ?attrs ?on_icon icon () =
    button ~a:([ a_class (base_class :: classes) ]
               <@> attrs)
      (on_icon ^:: icon :: [])

end
