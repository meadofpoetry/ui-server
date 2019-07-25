open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Pc_control_types

let name = "IPV4"

let failwith s = failwith @@ Printf.sprintf "%s: %s" name s

module Selector = struct
  let dhcp = Printf.sprintf "#%s" Settings_section.IPV4.dhcp_id
  let ip = Printf.sprintf "#%s" Settings_section.IPV4.ip_address_input_id
  let mask = Printf.sprintf "#%s" Settings_section.IPV4.mask_input_id
  let gateway = Printf.sprintf "#%s" Settings_section.IPV4.gateway_input_id
end

class t (elt : Dom_html.element Js.t) = object

  val dhcp : Switch.t Form_field.t =
    match Element.query_selector elt Selector.dhcp with
    | None -> failwith "dhcp input field not found"
    | Some x -> Form_field.attach Switch.attach x

  val ip : 'a Textfield.t =
    match Element.query_selector elt Selector.ip with
    | None -> failwith "ip address input field not found"
    | Some x -> Textfield.attach x

  val mask : 'a Textfield.t =
    match Element.query_selector elt Selector.mask with
    | None -> failwith "subnet mask input field not found"
    | Some x -> Textfield.attach x

  val gateway : 'a Textfield.t =
    match Element.query_selector elt Selector.gateway with
    | None -> failwith "gateway input field not found"
    | Some x -> Textfield.attach x

  inherit Widget.t elt () as super

  method! destroy () : unit =
    ip#destroy ();
    mask#destroy ();
    gateway#destroy ();
    super#destroy ()

end

let make (init : Network_config.ipv4_conf) : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Settings_section.IPV4.make init in
  new t elt
