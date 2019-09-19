open Js_of_ocaml_tyxml
include Components_tyxml.Divider
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

let attach = Widget.create

let make_hr ?classes ?a ?inset () =
  D.divider_hr ?classes ?a ?inset () |> Tyxml_js.To_dom.of_hr |> attach

let make_li ?classes ?a ?inset () =
  D.divider_li ?classes ?a ?inset () |> Tyxml_js.To_dom.of_li |> attach
