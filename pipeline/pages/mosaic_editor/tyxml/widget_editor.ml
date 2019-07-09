open Components_tyxml

module CSS = struct
  let grid = "widget-grid"
  let grid_item = BEM.add_element grid "item"
  let grid_item_content = BEM.add_element grid "item-content"
  let grid_ghost = BEM.add_element grid "ghost"
  let grid_overlay = BEM.add_element grid "overlay"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html
  open Utils

  module Card' = Card.Make(Xml)(Svg)(Html)

  let create_grid_overlay ?(classes = []) ?attrs () =
    let classes = CSS.grid_overlay :: classes in
    canvas ~a:([a_class classes] <@> attrs) []

  let create_grid_ghost ?(classes = []) ?attrs () =
    let classes = CSS.grid_ghost :: classes in
    div ~a:([a_class classes] <@> attrs) []

  let create_grid ?(classes = []) ?attrs ?(content = []) () =
    let classes = CSS.grid :: classes in
    div ~a:([ a_class classes
            ; a_role ["grid"] ])
      content

end
