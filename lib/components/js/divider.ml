include Components_tyxml.Divider
module Markup_js =
  Components_tyxml.Divider.Make
    (Js_of_ocaml_tyxml.Tyxml_js.Xml)
    (Js_of_ocaml_tyxml.Tyxml_js.Svg)
    (Js_of_ocaml_tyxml.Tyxml_js.Html)

let make ?classes ?attrs ?inset () : Widget.t =
  Widget.create
  @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
  @@ Markup_js.create ?classes ?attrs ?inset ()

let attach = Widget.create
