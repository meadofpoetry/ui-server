open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Pc_control_types

class t (elt : Dom_html.element Js.t) = object

  inherit Widget.t elt () as _super

end

let make (init : Network_config.ipv4_conf) : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Settings_section.Routes.make init in
  new t elt
