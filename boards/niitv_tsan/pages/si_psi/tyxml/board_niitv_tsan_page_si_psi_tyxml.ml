open Components_tyxml
open Application_types
open Board_niitv_tsan_types
open Board_niitv_tsan_widgets_tyxml

module CSS = struct
  let root = Printf.sprintf "%s-page-si-psi" (Topology.make_dom_node_class board_id)
end

let id control = Printf.sprintf "board-%d-si-psi" control

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Si_psi_overview_markup = Si_psi_overview.Make (Xml) (Svg) (Html)
  module Layout_grid_markup = Layout_grid.Make (Xml) (Svg) (Html)

  let create ?(classes = []) ?(attrs = []) ?children ~control () =
    let classes = CSS.root :: Layout_grid.CSS.inner :: classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          Layout_grid_markup.
            [ create_cell
                ~span:12
                ~children:[Si_psi_overview_markup.create ~control ()]
                () ]
    in
    div ~a:([a_id (id control); a_class classes] @ attrs) children
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
