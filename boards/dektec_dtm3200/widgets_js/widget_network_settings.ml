open Js_of_ocaml
open Application_types
open Board_dektec_dtm3200_types
open Board_dektec_dtm3200_http_js
open Components

let base_class = "ip-dektec-network-settings"

let ipv4 = Textfield.(
    Custom { input_type = `Text
           ; to_string = Ipaddr.V4.to_string
           ; of_string = fun x ->
               match Ipaddr.V4.of_string x with
               | Error `Msg s -> Error s
               | Ok _ as x -> x
           })

let make_ip ~id ~label () =
  let signal, push = React.S.create None in
  let ip = Textfield.make_textfield ~input_id:id ~label ipv4 in
  let listener =
    Events.inputs ip#input_element (fun _ _ ->
        push ip#value;
        Lwt.return_unit) in
  ip#set_on_destroy (fun () -> Lwt.cancel listener);
  ip, signal

let make_dhcp (inputs : 'a #Textfield.t list) =
  let signal, push = React.S.create false in
  let dhcp = Switch.make ~on_change:(fun x ->
      push x#checked;
      List.iter (fun w -> w#set_disabled x#checked) inputs;
      Lwt.return_unit)
      () in
  let form =
    Form_field.make
      ~label:"DHCP"
      ~align_end:true
      dhcp in
  form, signal

let name = "Настройки. Сеть"

let settings = None

let ( >>= ) = Lwt.( >>= )

type event =
  [ `Nw_mode of nw
  | `State of Topology.state
  ]

class t (state : Topology.state) (mode : nw) (control : int) =
  let s_state, set_state = React.S.create state in
  let ip, s_ip = make_ip ~id:"ip" ~label:"IP адрес" () in
  let mask, s_mask = make_ip ~id:"mask" ~label:"Маска подсети" () in
  let gw, s_gw = make_ip ~id:"gw" ~label:"Шлюз" () in
  let dhcp, s_dhcp = make_dhcp [ip; mask; gw] in
  let submit = Button.make ~label:"Применить" () in
  let buttons = Card.Actions.make_buttons [submit] in
  let actions = Card.Actions.make [buttons] in
  let (s : nw option React.signal) =
    React.S.l5 ~eq:(Util_equal.Option.equal equal_nw)
      (fun ip mask gw dhcp state ->
         match ip, mask, gw, state with
         | Some ip_address, Some mask, Some gw, `Fine ->
           Some { ip_address; mask; gateway = gw; dhcp }
         | _ -> None)
      s_ip s_mask s_gw s_dhcp s_state in
  object(self)
    val mutable _on_submit = None
    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#append_child dhcp;
      super#append_child ip;
      super#append_child mask;
      super#append_child gw;
      super#append_child actions;
      super#add_class base_class;
      self#set_value mode;
      _on_submit <- Some (Events.clicks submit#root (fun _ _ ->
          self#request_ (assert false)
          >>= fun _ -> Lwt.return ()))

    method! destroy () : unit =
      super#destroy ();
      React.S.stop ~strong:true s;
      (match _on_submit with
       | None -> ()
       | Some x -> Lwt.cancel x; _on_submit <- None)

    method notify : event -> unit = function
      | `State x ->
        set_state x;
        let disabled = match x with `Fine -> false | _ -> true in
        dhcp#input#set_disabled disabled;
        ip#set_disabled disabled;
        mask#set_disabled disabled;
        gw#set_disabled disabled
      | `Nw_mode mode -> self#set_value mode

    method value : nw option =
      match ip#value, mask#value, gw#value with
      | Some ip_address, Some mask, Some gateway ->
        Some { dhcp = dhcp#input#checked
             ; ip_address
             ; mask
             ; gateway
             }
      | _ -> None

    method set_value (x : nw) =
      dhcp#input#toggle ~notify:true ~force:x.dhcp ();
      ip#set_value x.ip_address;
      mask#set_value x.mask;
      gw#set_value x.gateway

    method private request_ (x : nw) =
      let req = Http_network.(
          let ( >>= ) = Lwt_result.( >>= ) in
          submit#set_loading true;
          set_ip_address x.ip_address control
          >>= fun ip_address -> set_subnet_mask x.mask control
          >>= fun mask -> set_gateway x.gateway control
          >>= fun gateway -> set_dhcp x.dhcp control
          >>= fun dhcp ->
          if equal_nw x { ip_address; mask; gateway; dhcp }
          then Lwt.return_ok ()
          else reboot control) in
      submit#set_loading_lwt req;
      req

  end

let make state mode control =
  new t state mode control
