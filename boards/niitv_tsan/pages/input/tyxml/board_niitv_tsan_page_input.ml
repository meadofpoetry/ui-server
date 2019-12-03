let id = Board_niitv_tsan_types.board_id

module CSS = struct
  let root =
    Printf.sprintf "%s-input-page" (Application_types.Topology.make_dom_node_class id)
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create ?(classes = []) ?(a = []) ?(children = []) ~control () =
    let classes = CSS.root :: classes in
    div ~a:(a_id (Printf.sprintf "board-%d" control) :: a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
