open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  module CSS = Side_sheet.Make_css(struct let root = "mdc-drawer" end)

  let create_scrim ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.scrim :: classes in
    div ~a:([a_class classes] <@> attrs) []

  let create_content ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.content :: classes in
    div ~a:([a_class classes] <@> attrs) content

  let create ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.root :: classes in
    aside ~a:([a_class classes] <@> attrs) [content]

end
