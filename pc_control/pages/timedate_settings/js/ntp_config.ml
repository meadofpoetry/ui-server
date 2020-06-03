open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
include Page_timedate_settings_tyxml.NTP
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let name = "NTP"

module Selector = struct
  let ntp = "#" ^ ntp_id

  let ntp_server = "#" ^ ntp_server_id

  let ntp_address = "#" ^ ntp_address_id
end

class t (elt : Dom_html.element Js.t) =
  let signal, push = React.S.create false in
  object (self)
    val ntp : Switch.t Form_field.t =
      let ntp_elt = Element.query_selector_exn elt Selector.ntp in
      Form_field.attach Switch.attach ntp_elt

    val ntp_server : string Textfield.t =
      let ntp_server_elt = Element.query_selector_exn elt Selector.ntp_server in
      Textfield.attach ntp_server_elt

    val ntp_address : string Textfield.t =
      let ntp_address_elt = Element.query_selector_exn elt Selector.ntp_address in
      Textfield.attach ntp_address_elt

    inherit Widget.t elt () as super

    val mutable _listeners = []

    val mutable _value = None

    val _signal = signal

    method! init () : unit =
      self#handle_ntp_change;
      _value <- self#value;
      super#init ()

    method! initial_sync_with_dom () : unit =
      _listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.
          [
            changes ntp#input#input_element (fun _ _ ->
                self#handle_ntp_change;
                Lwt.return_unit);
          ];
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      ntp#destroy ();
      ntp_server#destroy ();
      ntp_address#destroy ();
      super#destroy ()

    method set_value
             ((flag, serv, addr) : (bool * string option * Netlib.Ipaddr.V4.t option)) =
      ntp#input#toggle ~force:flag ();
      (match serv with
       | None ->
          ntp_server#set_value ""
       | Some serv ->
          ntp_server#set_value serv);
      (match addr with
       | None ->
          ntp_address#set_value ""
       | Some addr ->
          ntp_address#set_value (Ipaddr.V4.to_string addr));
      self#handle_ntp_change;
      _value <- Some (flag, serv, addr)

    method value =
      let get_addr a =
        try Option.map Ipaddr.V4.of_string_exn a
        with _ -> None
      in
      match _value with
      | None ->
         let ntp_flag = ntp#input#checked in
         let server = ntp_server#value in
         let address = ntp_address#value in
         Some (ntp_flag, server, get_addr address)
      | Some (_,serv,addr) ->
         Some (ntp#input#checked, serv, addr)

    method private handle_ntp_change =
      let disabled = ntp#input#checked in
      push disabled

    method disabled = signal

  end

let make (init : Pc_control_types.Timedate_config.t) : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element @@ D.create init
  in
  new t elt
