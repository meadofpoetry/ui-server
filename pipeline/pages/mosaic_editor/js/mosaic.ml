open Js_of_ocaml
open Components

class t (elt : Dom_html.element Js.t) () =
  object
    inherit Widget.t elt () as super

  end
