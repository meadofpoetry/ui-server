open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components
open Pc_control_types

let name = "IPV4"

let failwith s = failwith @@ Printf.sprintf "%s: %s" name s

module Selector = struct
  let dhcp = Printf.sprintf "#%s" Markup.IPV4.dhcp_id

  let ip = Printf.sprintf "#%s" Markup.IPV4.ip_address_input_id

  let mask = Printf.sprintf "#%s" Markup.IPV4.mask_input_id

  let gateway = Printf.sprintf "#%s" Markup.IPV4.gateway_input_id
end

class t (elt : Dom_html.element Js.t) =
  object (self)
    val dhcp : Switch.t Form_field.t =
      match Element.query_selector elt Selector.dhcp with
      | None -> failwith "dhcp input field not found"
      | Some x -> Form_field.attach Switch.attach x

    val ip : 'a Textfield.t =
      match Element.query_selector elt Selector.ip with
      | None -> failwith "ip address input field not found"
      | Some x -> Textfield.attach ~validation:Util.ipv4_validation x

    val mask : int32 Textfield.t =
      match Element.query_selector elt Selector.mask with
      | None -> failwith "subnet mask input field not found"
      | Some x -> Textfield.attach ~validation:Util.mask_validation x

    val gateway : 'a Textfield.t =
      match Element.query_selector elt Selector.gateway with
      | None -> failwith "gateway input field not found"
      | Some x -> Textfield.attach ~validation:Util.ipv4_validation x

    inherit Widget.t elt () as super

    val mutable _listeners = []

    val mutable _value = None

    method! init () : unit =
      self#handle_dhcp_change ();
      _value <- self#value;
      super#init ()

    method! initial_sync_with_dom () : unit =
      _listeners <-
        Lwt_js_events.
          [ changes (dhcp#input)#input_element (fun _ _ ->
                self#handle_dhcp_change ();
                Lwt.return_unit) ];
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      ip#destroy ();
      mask#destroy ();
      gateway#destroy ();
      super#destroy ()

    method set_value ({address; routes; meth; _} as v : Network_config.ipv4_conf) =
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

    method value : Network_config.ipv4_conf option =
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
            { Network_config.meth = Auto
            ; address
            ; routes = {gateway; static = []}
            ; dns = [] }
      | Some ip, Some mask, false ->
          Some
            { Network_config.meth = Manual
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

let make (init : Network_config.ipv4_conf) : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element @@ Markup.IPV4.make init
  in
  new t elt
