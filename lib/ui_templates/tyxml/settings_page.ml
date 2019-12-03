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

  let create_section_title ?(classes = []) ?(a = []) ?title ?(children = []) () =
    let classes = CSS.section_title :: classes in
    h2 ~a:(a_class classes :: a) (Utils.map_cons_option txt title children)

  let create_section_header ?(classes = []) ?(a = []) ?title ?(children = []) () =
    let classes = CSS.section_header :: classes in
    let title =
      match title with
      | None -> None
      | Some (`Text s) -> Some (create_section_title ~title:s ())
      | Some (`Element e) -> Some e
    in
    div ~a:(a_class classes :: a) Utils.(title ^:: children)

  let create_section ?(classes = []) ?(a = []) ~header ?children () : 'a elt =
    let classes = CSS.section :: classes in
    div ~a:(a_class classes :: a) [header; Card_markup.card ?children ()]

  let create ?(classes = []) ?(a = []) ?(children = []) () : 'a elt =
    let classes = CSS.root :: Layout_grid.CSS.root :: classes in
    div ~a:(a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
