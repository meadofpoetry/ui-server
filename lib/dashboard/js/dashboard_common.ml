open Js_of_ocaml_tyxml

module Markup = Dashboard_tyxml.Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)
