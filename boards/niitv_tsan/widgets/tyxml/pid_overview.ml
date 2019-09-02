open Components_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-pid-overview"

  let row = BEM.add_element root "row"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
end
