open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Components
include Board_niitv_tsan_widgets_tyxml.Pid_overview
module D = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)

type event = [`State of [Topology.state | `No_sync]]

class t (elt : Dom_html.element Js.t) () =
  object
    inherit [int] Table_overview.t elt ()
  end

let attach elt : t = new t (elt : Dom_html.element Js.t) ()

let make ?a ?dense ?init ?hex ~control () =
  D.create ?a ?dense ?init ?hex ~control () |> Tyxml_js.To_dom.of_div |> attach
