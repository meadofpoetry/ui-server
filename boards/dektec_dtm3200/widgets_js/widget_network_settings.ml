open Js_of_ocaml
open Application_types
open Board_dektec_dtm3200_types
open Board_dektec_dtm3200_http_js
open Components

type event =
  [ `Mode of nw
  | `State of Topology.state ]

let base_class = "ip-dektec-network-settings"

let ipv4 =
  Textfield.(
    Custom
      { input_type = `Text
      ; to_string = Ipaddr.V4.to_string
      ; of_string =
          (fun x ->
            match Ipaddr.V4.of_string x with
            | Error (`Msg s) -> Error s
            | Ok _ as x -> x) })

let make_ip ~id ~label value =
  let event, push = React.E.create () in
  let ip = Textfield.make ~input_id:id ~label ~value ~validation:ipv4 () in
  let listener =
    Js_of_ocaml_lwt.Lwt_js_events.inputs ip#input_element (fun _ _ ->
        push ();
        Lwt.return_unit)
  in
  ip#set_on_destroy (fun () -> Lwt.cancel listener);
  ip, event

let make_dhcp checked (inputs : 'a #Textfield.t list) =
  let event, push = React.E.create () in
  let dhcp =
    Switch.make
      ~checked
      ~on_change:(fun x ->
        push ();
        List.iter (fun w -> w#set_disabled x#checked) inputs;
        Lwt.return_unit)
      ()
  in
  let form = Form_field.make_of_widget ~label:"DHCP" ~align_end:true ~input:dhcp () in
  form, event

class t (state : Topology.state) (mode : nw) (control : int) =
  let ip, e_ip = make_ip ~id:"ip" ~label:"IP адрес" mode.ip_address in
  let mask, e_mask = make_ip ~id:"mask" ~label:"Маска подсети" mode.mask in
  let gw, e_gw = make_ip ~id:"gw" ~label:"Шлюз" mode.gateway in
  let dhcp, e_dhcp = make_dhcp mode.dhcp [ip; mask; gw] in
  let submit = Button.make ~label:"Применить" () in
  let buttons = Card.D.card_action_buttons ~children:[submit#markup] () in
  let actions = Card.D.card_actions ~children:[buttons] () in
  object (self)
    val mutable _on_submit = None

    val mutable _e_change = None

    val mutable _dhcp = mode.dhcp

    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#append_child dhcp;
      super#append_child ip;
      super#append_child mask;
      super#append_child gw;
      Dom.appendChild super#root @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element actions;
      super#add_class base_class;
      super#add_class Box.CSS.root;
      super#add_class Box.CSS.vertical;
      self#notify (`State state);
      _e_change <-
        Some
          (React.E.map self#update_submit_button_state
          @@ React.E.select [e_ip; e_mask; e_gw; e_dhcp]);
      _on_submit <-
        Some
          (Js_of_ocaml_lwt.Lwt_js_events.clicks submit#root (fun _ _ ->
               Lwt.map ignore @@ self#submit ()))

    method! destroy () : unit =
      super#destroy ();
      Option.iter (React.E.stop ~strong:true) _e_change;
      match _on_submit with
      | None -> ()
      | Some x ->
          Lwt.cancel x;
          _on_submit <- None

    method submit () : (unit, [`Msg of string]) Lwt_result.t =
      match self#value with
      | None -> Lwt.return_error (`Msg "Please fill the settings form")
      | Some mode ->
          let req =
            Http_network.(
              let ( >>= ) = Lwt_result.( >>= ) in
              submit#set_loading true;
              set_ip_address mode.ip_address control
              >>= fun ip_address ->
              set_subnet_mask mode.mask control
              >>= fun mask ->
              set_gateway mode.gateway control
              >>= fun gateway ->
              set_dhcp mode.dhcp control
              >>= fun dhcp ->
              if equal_nw mode {ip_address; mask; gateway; dhcp} && _dhcp = dhcp
              then Lwt.return_ok ()
              else reboot control)
          in
          submit#set_loading_lwt req;
          req

    method value : nw option =
      let disabled = (dhcp#input)#disabled in
      match disabled, ip#value, mask#value, gw#value with
      | false, Some ip_address, Some mask, Some gateway ->
          Some {dhcp = (dhcp#input)#checked; ip_address; mask; gateway}
      | _ -> None

    method set_value (x : nw) =
      (dhcp#input)#toggle ~notify:true ~force:x.dhcp ();
      ip#set_value x.ip_address;
      mask#set_value x.mask;
      gw#set_value x.gateway;
      self#update_submit_button_state ()

    method notify : event -> unit =
      function
      | `Mode mode -> self#set_value mode
      | `State x ->
          let disabled =
            match x with
            | `Fine -> false
            | _ -> true
          in
          (dhcp#input)#set_disabled disabled;
          let disabled = (dhcp#input)#checked || disabled in
          ip#set_disabled disabled;
          mask#set_disabled disabled;
          gw#set_disabled disabled;
          self#update_submit_button_state ()

    method private update_submit_button_state () =
      match self#value with
      | Some _ -> submit#set_disabled false
      | None -> submit#set_disabled true
  end

let make state mode control = new t state mode control
