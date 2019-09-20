open Components_tyxml

module CSS = struct
  let root = "grid-overlay"

  let snap_line = root ^ "-snap-line"

  let snap_line_center_multiple = BEM.add_modifier snap_line "center-multiple"

  let snap_line_center = BEM.add_modifier snap_line "center"

  let snap_line_multiple = BEM.add_modifier snap_line "multiple"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create ?(classes = []) ?(a = []) ?(size = 10) () : 'a elt =
    let classes = CSS.root :: classes in
    canvas ~a:(a_class classes :: a_user_data "size" (string_of_int size) :: a) []
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
