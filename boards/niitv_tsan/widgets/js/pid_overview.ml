open Js_of_ocaml
open Application_types
open Components
include Board_niitv_tsan_widgets_tyxml.Pid_overview
module D = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)

type event = [`State of [Topology.state | `No_sync]]

class t ?set_hex (elt : Dom_html.element Js.t) () =
  object
    inherit [int] Table_overview.t ?set_hex elt ()
  end

let attach ?set_hex elt : t = new t ?set_hex (elt : Dom_html.element Js.t) ()
