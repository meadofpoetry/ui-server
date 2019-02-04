open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  module CSS = struct
    include Side_sheet.Make_css(struct let root = "mdc-drawer" end)
    let header = add_element root "header"
    let title = add_element root "title"
    let subtitle = add_element root "subtitle"
  end

  let create_scrim ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.scrim :: classes in
    div ~a:([a_class classes] <@> attrs) []

  let create_title ?(tag = h3) ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.title :: classes in
    tag ~a:([a_class classes] <@> attrs) content

  let create_subtitle ?(tag = h6) ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.subtitle :: classes in
    tag ~a:([a_class classes] <@> attrs) content

  let create_header ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.header :: classes in
    div ~a:([a_class classes] <@> attrs) content

  let create_content ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.content :: classes in
    div ~a:([a_class classes] <@> attrs) content

  let create ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.root :: classes in
    aside ~a:([a_class classes] <@> attrs) [content]

end
