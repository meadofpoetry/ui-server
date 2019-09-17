open Js_of_ocaml
open Js_of_ocaml_tyxml
include Board_niitv_tsan_widgets_tyxml.Si_psi_overview
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
