open Application_types
open Board_dektec_dtm3200_types
open Board_dektec_dtm3200_http_js
open Components

let base_class = "ip-dektec-network-settings"

let make_ip () =
  let ip =
    new Textfield.t
      ~input_id:"ip"
      ~input_type:IPV4
      ~label:"IP адрес" () in
  let set (x : nw) = ip#set_value x.ip_address in
  ip#widget, set, ip#s_input, ip#set_disabled

let make_mask () =
  let mask =
    new Textfield.t
      ~input_id:"mask"
      ~input_type:IPV4
      ~label:"Маска подсети" () in
  let set (x : nw) = mask#set_value x.mask in
  mask#widget, set, mask#s_input, mask#set_disabled

let make_gateway () =
  let gw =
    new Textfield.t
      ~input_id:"gw"
      ~input_type:IPV4
      ~label:"Шлюз" () in
  let set (x : nw) = gw#set_value x.gateway in
  gw#widget, set, gw#s_input, gw#set_disabled

let make_dhcp () =
  let dhcp = new Switch.t () in
  let form =
    new Form_field.t
      ~input:dhcp
      ~label:"DHCP"
      ~align_end:true () in
  let set (x : nw) = dhcp#set_checked x.dhcp in
  form#widget,set,dhcp#s_state,dhcp#set_disabled

let name = "Настройки. Сеть"

let settings = None

let ( >>= ) = Lwt_result.( >>= )

let make ~(state : Topology.state React.signal)
         ~(mode : nw React.signal)
         control =
  let ip, set_ip, s_ip, dis_ip = make_ip () in
  let mask, set_mask, s_mask, dis_mask = make_mask () in
  let gw, set_gw, s_gw, dis_gw = make_gateway () in
  let dhcp, set_dhcp, s_dhcp, dis_dhcp = make_dhcp () in
  let (s : nw option React.signal) =
    React.S.l5 ~eq:(Util_equal.Option.equal equal_nw)
      (fun ip mask gw dhcp state ->
        match ip, mask, gw, state with
        | Some ip_address, Some mask, Some gw, `Fine ->
           Some { ip_address; mask; gateway = gw; dhcp }
        | _ -> None)
      s_ip s_mask s_gw s_dhcp state in
  let s_set =
    React.S.map ~eq:(=) (fun x ->
        List.iter (fun f -> f x)
          [set_dhcp; set_ip; set_mask; set_gw]) mode in
  let s_dis =
    React.S.l2 ~eq:(=) (fun state dhcp ->
        let is_disabled = match state,dhcp with
          | `Fine, false -> false
          | _ -> true in
        List.iter (fun f -> f is_disabled) [dis_ip; dis_mask; dis_gw];
        dis_dhcp (match state with `Fine -> false | _ -> true))
      state s_dhcp in
  let submit (x : nw) = Http_network.(
      set_ip_address x.ip_address control
      >>= fun ip_address -> set_subnet_mask x.mask control
      >>= fun mask -> set_gateway x.gateway control
      >>= fun gateway -> set_dhcp x.dhcp control
      >>= fun dhcp ->
      if equal_nw x { ip_address; mask; gateway; dhcp }
      then Lwt.return_ok ()
      else reboot control) in
  let apply = new Ui_templates.Buttons.Set.t s submit () in
  let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
  let actions = new Card.Actions.t ~widgets:[buttons] () in
  let box = new Vbox.t ~widgets:[dhcp; ip; mask; gw; actions#widget] () in
  box#set_on_destroy (fun () ->
      React.S.stop ~strong:true s_set;
      React.S.stop ~strong:true s_dis);
  box#add_class base_class;
  box#widget
