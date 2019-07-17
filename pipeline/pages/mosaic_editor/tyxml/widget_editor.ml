open Components_tyxml

module CSS = struct
  let root = "widget-grid"
  let item = BEM.add_element root "item"
  let item_content = BEM.add_element root "item-content"
  let item_dragging = BEM.add_modifier item "dragging"
  let ghost = BEM.add_element root "ghost"
  let overlay = BEM.add_element root "overlay"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html
  open Utils

  module Card' = Card.Make(Xml)(Svg)(Html)

  let create_overlay ?(classes = []) ?attrs () =
    let classes = CSS.overlay :: classes in
    canvas ~a:([a_class classes] <@> attrs) []

  let create_ghost ?(classes = []) ?attrs () =
    let classes = CSS.ghost :: classes in
    div ~a:([a_class classes] <@> attrs) []

  let create_grid ?(classes = []) ?attrs ?(content = []) () =
    let classes = CSS.root :: classes in
    div ~a:([ a_class classes
            ; a_role ["grid"] ])
      content

end
