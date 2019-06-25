open Components_tyxml

module CSS = struct
  let root = "mosaic"
  let video = BEM.add_element root "video"

  let grid = root ^ "-grid"
  let grid_item = BEM.add_element grid "item"
  let grid_item_content = BEM.add_element grid "item-content"
  let grid_ghost = BEM.add_element grid "ghost"
  let grid_overlay = BEM.add_element grid "overlay"


  module Container_grid = struct
    let root = "container-grid"
    let cell = BEM.add_element root "cell"
    let col_handle = BEM.add_element root "col-handle"
    let row_handle = BEM.add_element root "row-handle"
    let mul_handle = BEM.add_element root "mul-handle"
    let cell_dragging_column = BEM.add_modifier cell "dragging-column"
    let cell_dragging_row = BEM.add_modifier cell "dragging-row"
  end

  let resizable = "resizable"
  let resizable_active = BEM.add_modifier resizable "active"
  let resizers = BEM.add_element resizable "resizers"
  let resizer = BEM.add_element resizable "resizer"
  let resizer_top_left = BEM.add_modifier resizer "top-left"
  let resizer_top_right = BEM.add_modifier resizer "top-right"
  let resizer_bottom_left = BEM.add_modifier resizer "bottom-left"
  let resizer_bottom_right = BEM.add_modifier resizer "bottom-right"
  let resizer_top = BEM.add_modifier resizer "top"
  let resizer_bottom = BEM.add_modifier resizer "bottom"
  let resizer_left = BEM.add_modifier resizer "left"
  let resizer_right = BEM.add_modifier resizer "right"

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

  let create_grid_ghost ?(classes = []) ?attrs () =
    let classes = CSS.grid_ghost :: classes in
    div ~a:([a_class classes] <@> attrs) []

  let create_grid ?(classes = []) ?attrs ?(content = []) () =
    let classes = CSS.grid :: classes in
    div ~a:([ a_class classes
            ; a_role ["grid"] ])
      content

  let create_resizable ?(tabindex = -1) ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.resizable :: classes in
    div ~a:([ a_class classes
            ; a_tabindex tabindex ] <@> attrs)
      [ div ~a:[a_class [CSS.resizers]]
          [ div ~a:[a_class [CSS.resizer; CSS.resizer_top_left]] []
          ; div ~a:[a_class [CSS.resizer; CSS.resizer_top_right]] []
          ; div ~a:[a_class [CSS.resizer; CSS.resizer_bottom_left]] []
          ; div ~a:[a_class [CSS.resizer; CSS.resizer_bottom_right]] []
          ]
      ]

  module Container_grid = struct

    let create_cell ?(classes = []) ?attrs ?row ?col ?(content = []) () : 'a elt =
      let classes = CSS.Container_grid.cell :: classes in
      let style = match row, col with
        | None, None -> None
        | Some row, None -> Some (Printf.sprintf "grid-row: %d" row)
        | None, Some col -> Some (Printf.sprintf "grid-column: %d" col)
        | Some row, Some col ->
          Some (Printf.sprintf "grid-row: %d; grid-column: %d" col row)
      in
      div ~a:([a_class classes] <@> attrs
              |> map_cons_option a_style style
              |> map_cons_option (a_user_data "row" % string_of_int) row
              |> map_cons_option (a_user_data "col" % string_of_int) col)
        ([ div ~a:[a_class [CSS.Container_grid.row_handle]] []
         ; div ~a:[a_class [CSS.Container_grid.col_handle]] []
         ; div ~a:[a_class [CSS.Container_grid.mul_handle]] []
         ] @ content)

    let create ?(classes = []) ?attrs ?(content = []) () : 'a elt =
      let classes = CSS.Container_grid.root :: classes in
      div ~a:([a_class classes] <@> attrs) content
  end

end
