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

  let create ?(classes = []) ?(attrs = []) ?(children = []) ~control () =
    let classes = CSS.root :: classes in
    div ~a:([a_id (id control); a_class classes] @ attrs) children
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
