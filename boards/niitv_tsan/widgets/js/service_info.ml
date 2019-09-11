open Js_of_ocaml
open Components

class general_info (elt : Dom_html.element Js.t) =
  object
    inherit Widget.t elt ()
  end
