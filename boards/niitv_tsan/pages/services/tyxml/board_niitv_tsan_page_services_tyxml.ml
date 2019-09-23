open Components_tyxml
open Application_types
open Board_niitv_tsan_types

module CSS = struct
  let root = Printf.sprintf "%s-page-services" (Topology.make_dom_node_class board_id)
end

let id control = Printf.sprintf "board-%d-services" control

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create ?(classes = []) ?(a = []) ?(children = []) ~control () =
    let classes = CSS.root :: Layout_grid.CSS.inner :: classes in
    div ~a:(a_id (id control) :: a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
