open Utils

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-tab-bar"

  let create ?(classes=[]) ?attrs ~scroller () =
    div ~a:([ a_class (base_class :: classes)
            ; a_role  ["tablist"]] <@> attrs)
      [ scroller ]

end
