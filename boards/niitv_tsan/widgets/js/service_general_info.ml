open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
include Board_niitv_tsan_widgets_tyxml.Service_general_info
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

class t (elt : Dom_html.element Js.t) () =
  object
    inherit Widget.t elt ()
  end

let attach elt = new t (elt :> Dom_html.element Js.t) ()

let make ?classes ?attrs ?info ?children () =
  Markup_js.create ?classes ?attrs ?info ?children () |> Tyxml_js.To_dom.of_ul |> attach
