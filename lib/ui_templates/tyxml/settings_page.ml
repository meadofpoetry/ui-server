open Components_tyxml

module CSS = struct
  let root = "settings-page"

  let section = "settings-section"

  let section_title = BEM.add_element section "title"

  let section_header = BEM.add_element section "header"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  module Card_markup = Components_tyxml.Card.Make (Xml) (Svg) (Html)
  open Html

  let create_section_title ?(classes = []) ?(attrs = []) title : 'a elt =
    let classes = CSS.section_title :: classes in
    h2 ~a:([a_class classes] @ attrs) [txt title]

  let create_section_header ?(classes = []) ?(attrs = []) ?title content : 'a elt =
    let classes = CSS.section_header :: classes in
    div
      ~a:([a_class classes] @ attrs)
      (match title with
      | None -> content
      | Some x -> create_section_title x :: content)

  let create_section ?(classes = []) ?(attrs = []) ~header content : 'a elt =
    let classes = CSS.section :: classes in
    div ~a:([a_class classes] @ attrs) [header; Card_markup.create content]

  let create ?(classes = []) ?(attrs = []) content : 'a elt =
    let classes = CSS.root :: Layout_grid.CSS.root :: classes in
    div ~a:([a_class classes] @ attrs) content
end
