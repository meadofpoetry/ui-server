open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Card
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let attach (elt : #Dom_html.element Js.t) : Widget.t = Widget.create elt

let make ?tag ?classes ?attrs ?outlined ?children () =
  Markup_js.create ?tag ?classes ?attrs ?outlined ?children ()
  |> Tyxml_js.To_dom.of_element
  |> attach
