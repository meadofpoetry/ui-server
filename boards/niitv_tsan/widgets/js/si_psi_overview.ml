open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Si_psi_overview
module D = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)

class t ?set_hex (elt : Dom_html.element Js.t) () =
  object
    inherit [SI_PSI_table.id] Table_overview.t ?set_hex elt ()
  end

let attach ?set_hex elt : t = new t ?set_hex (elt :> Dom_html.element Js.t) ()
