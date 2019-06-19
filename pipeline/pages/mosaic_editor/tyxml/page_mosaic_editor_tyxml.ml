open Components_tyxml

module CSS = struct
  let root = "mosaic"
  let video = BEM.add_element root "video"

  let table = root ^ "-table"
  let table_item = BEM.add_element table "item"

  let grid_overlay = BEM.add_element root "grid-overlay"

  let resizable = "resizable"
  let resizable_active = BEM.add_modifier resizable "active"
  let resizers = BEM.add_element resizable "resizers"
  let resizer = BEM.add_element resizable "resizer"
  let resizer_top_left = BEM.add_modifier resizer "top-left"
  let resizer_top_right = BEM.add_modifier resizer "top-right"
  let resizer_bottom_left = BEM.add_modifier resizer "bottom-left"
  let resizer_bottom_right = BEM.add_modifier resizer "bottom-right"

  let divider = "mosaic-table-divider"
  let divider_inner = BEM.add_element divider "inner"
  let divider_horizontal = BEM.add_modifier divider "horizontal"
  let divider_vertical = BEM.add_modifier divider "vertical"

  let top_app_bar_contextual = BEM.add_modifier Top_app_bar.CSS.root "contextual-action"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html
  open Utils

  let create_divider ?(classes = []) ?attrs ?(vertical = false) () : 'a elt =
    let dir_class =
      if vertical
      then CSS.divider_vertical
      else CSS.divider_horizontal in
    let classes = [CSS.divider; dir_class] @ classes in
    div ~a:([a_class classes] <@> attrs)
      [div ~a:[a_class [CSS.divider_inner]] []]

  let create_grid_overlay ?(classes = []) ?attrs () =
    let classes = CSS.grid_overlay :: classes in
    canvas ~a:([a_class classes] <@> attrs) []

  let create_resizable ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.resizable :: classes in
    div ~a:([a_class classes] <@> attrs)
      [ div ~a:[a_class [CSS.resizers]]
          [ div ~a:[a_class [CSS.resizer; CSS.resizer_top_left]] []
          ; div ~a:[a_class [CSS.resizer; CSS.resizer_top_right]] []
          ; div ~a:[a_class [CSS.resizer; CSS.resizer_bottom_left]] []
          ; div ~a:[a_class [CSS.resizer; CSS.resizer_bottom_right]] []
          ]
      ]

  let create_table_item ?(classes = []) ?attrs ?(content = []) () : 'a elt =
    let classes = CSS.table_item :: classes in
    div ~a:([a_class classes] <@> attrs) content
end
