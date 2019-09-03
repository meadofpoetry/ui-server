open Js_of_ocaml
open Js_of_ocaml_tyxml

include Components_tyxml.Divider
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

class t (elt : Dom_html.element Js.t) () =
object
  inherit Widget.t elt () as super

  method inset : bool =
    super#has_class CSS.inset

  method set_inset (x : bool) : unit =
    super#toggle_class ~force:x CSS.inset
end

let make ?inset () : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?inset () in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
