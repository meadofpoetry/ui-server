open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

let name = "HTTPS"

module Selector = struct
  let enable = Printf.sprintf "#%s" Markup.HTTPS.enable_id
end

class t (elt : Dom_html.element Js.t) = object

  val enable : Switch.t Form_field.t =
    match Element.query_selector elt Selector.enable with
    | None -> failwith @@ name ^ ": enable input field not found"
    | Some x -> Form_field.attach Switch.attach x

  inherit Widget.t elt () as super

  method! destroy () : unit =
    enable#destroy ();
    super#destroy ()
end

let make (init : Server_types.settings) : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.HTTPS.make init in
  new t elt
