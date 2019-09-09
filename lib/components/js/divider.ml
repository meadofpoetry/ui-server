open Js_of_ocaml_tyxml
include Components_tyxml.Divider
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let attach = Widget.create

let make_hr ?classes ?attrs ?inset () =
  Markup_js.create_hr ?classes ?attrs ?inset () |> Tyxml_js.To_dom.of_hr |> attach

let make_li ?classes ?attrs ?inset () =
  Markup_js.create_li ?classes ?attrs ?inset () |> Tyxml_js.To_dom.of_li |> attach
