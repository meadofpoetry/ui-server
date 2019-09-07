include Components_tyxml.Card
module Markup_js =
  Components_tyxml.Card.Make
    (Js_of_ocaml_tyxml.Tyxml_js.Xml)
    (Js_of_ocaml_tyxml.Tyxml_js.Svg)
    (Js_of_ocaml_tyxml.Tyxml_js.Html)

let make ?classes ?attrs ?outlined ?tag content : Widget.t =
  let elt =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
    @@ Markup_js.create ?classes ?attrs ?outlined ?tag
    @@ List.map Js_of_ocaml_tyxml.Tyxml_js.Of_dom.of_element content
  in
  Widget.create elt

let attach (elt : #Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t) : Widget.t =
  Widget.create elt
