open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Pc_control_types
include Page_network_settings_tyxml.Ethernet
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let validation =
  Textfield.(
    let of_string x =
      match Macaddr.of_string x with
      | Ok _ as v -> v
      | Error (`Msg m) -> Error m
    in
    Custom {of_string; to_string = Macaddr.to_string; input_type = `Text})

module Selector = struct
  let mac = Printf.sprintf "#%s" D.mac_input_id
end

class t (elt : Dom_html.element Js.t) =
  object
    val mac : 'a Textfield.t =
      match Element.query_selector elt Selector.mac with
      | None -> failwith "mac address input field not found"
      | Some x -> Textfield.attach ~validation x

    inherit Widget.t elt ()

    method set_value (x : Network_config.ethernet_conf) : unit =
      mac#set_value x.mac_address

    method value : Network_config.ethernet_conf option =
      match mac#value with
      | None -> None
      | Some mac_address -> Some {Network_config.mac_address}
  end

let make (init : Network_config.ethernet_conf) : t =
  let (elt : Dom_html.element Js.t) =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element @@ D.create init
  in
  new t elt
