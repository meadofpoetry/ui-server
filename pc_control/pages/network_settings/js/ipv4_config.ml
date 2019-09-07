open Js_of_ocaml
open Components
include Page_network_settings_tyxml.Ipv4
module Markup_js =
  Page_network_settings_tyxml.Ipv4.Make
    (Js_of_ocaml_tyxml.Tyxml_js.Xml)
    (Js_of_ocaml_tyxml.Tyxml_js.Svg)
    (Js_of_ocaml_tyxml.Tyxml_js.Html)

let name = "IPV4"

module Selector = struct
  let dhcp = Printf.sprintf "#%s" dhcp_id

  let ip = Printf.sprintf "#%s" ip_address_input_id

  let mask = Printf.sprintf "#%s" mask_input_id

  let gateway = Printf.sprintf "#%s" gateway_input_id
end

class t (elt : Dom_html.element Js.t) =
  object (self)
    val dhcp : Switch.t Form_field.t =
      let dhcp_elt = Element.query_selector_exn elt Selector.dhcp in
      Form_field.attach Switch.attach dhcp_elt

    val ip : 'a Textfield.t =
      let ip_elt = Element.query_selector_exn elt Selector.ip in
      Textfield.attach ~validation:Util.ipv4_validation ip_elt

    val mask : int32 Textfield.t =
      let mask_elt = Element.query_selector_exn elt Selector.mask in
      Textfield.attach ~validation:Util.mask_validation mask_elt

    val gateway : 'a Textfield.t =
      let gateway_elt = Element.query_selector_exn elt Selector.gateway in
      Textfield.attach ~validation:Util.ipv4_validation gateway_elt

    inherit Widget.t elt () as super

    val mutable _listeners = []

    val mutable _value = None

    method! init () : unit =
      self#handle_dhcp_change ();
      _value <- self#value;
      super#init ()

    method! initial_sync_with_dom () : unit =
      _listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.
          [ changes (dhcp#input)#input_element (fun _ _ ->
                self#handle_dhcp_change ();
                Lwt.return_unit) ];
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      ip#destroy ();
      mask#destroy ();
      gateway#destroy ();
      super#destroy ()

    method set_value
        ({address; routes; meth; _} as v : Pc_control_types.Network_config.ipv4_conf) =
      (dhcp#input)#toggle
        ~force:
          (match meth with
          | Auto -> true
          | Manual -> false)
        ();
      ip#set_value @@ fst address;
      mask#set_value @@ snd address;
      (match routes.gateway with
      | None -> gateway#clear ()
      | Some x -> gateway#set_value x);
      self#handle_dhcp_change ();
      _value <- Some v

    method value : Pc_control_types.Network_config.ipv4_conf option =
      match ip#value, mask#value, (dhcp#input)#checked with
      | _, _, true ->
          let get_or x = function
            | None -> x
            | Some v -> v
          in
          let address =
            match _value with
            | Some v -> v.address
            | None -> get_or Ipaddr.V4.any ip#value, get_or 24l mask#value
          in
          let gateway =
            match _value with
            | Some v -> v.routes.gateway
            | None -> gateway#value
          in
          Some
            { Pc_control_types.Network_config.meth = Auto
            ; address
            ; routes = {gateway; static = []}
            ; dns = [] }
      | Some ip, Some mask, false ->
          Some
            { Pc_control_types.Network_config.meth = Manual
            ; address = ip, mask
            ; routes = {gateway = gateway#value; static = []}
            ; dns = [] }
      | _ -> None

    method private handle_dhcp_change () =
      let disabled = (dhcp#input)#checked in
      ip#set_disabled disabled;
      mask#set_disabled disabled;
      gateway#set_disabled disabled
  end

let make (init : Pc_control_types.Network_config.ipv4_conf) : t =
  let (elt : Dom_html.element Js.t) =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element @@ Markup_js.create init
  in
  new t elt
