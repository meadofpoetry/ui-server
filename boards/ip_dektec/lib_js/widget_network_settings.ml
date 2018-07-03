open Board_types
open Containers
open Components
open Common

type config = unit [@@deriving yojson]

let make_ip () =
  let help_text : Textfield.Help_text.helptext = { validation=true;persistent=false;text=None} in
  let ip = new Textfield.t ~input_id:"ip" ~input_type:IPV4 ~help_text ~label:"IP адрес" () in
  let set (x:nw) = ip#fill_in x.ip in
  ip#widget,set,ip#s_input,ip#set_disabled

let make_mask () =
  let help_text : Textfield.Help_text.helptext = { validation=true;persistent=false;text=None} in
  let mask = new Textfield.t ~input_id:"mask" ~input_type:IPV4 ~help_text ~label:"Маска подсети" () in
  let set (x:nw) = mask#fill_in x.mask in
  mask#widget,set,mask#s_input,mask#set_disabled

let make_gateway () =
  let help_text : Textfield.Help_text.helptext = { validation=true;persistent=false;text=None} in
  let gw = new Textfield.t ~input_id:"gw" ~input_type:IPV4 ~help_text ~label:"Шлюз" () in
  let set (x:nw) = gw#fill_in x.gateway in
  gw#widget,set,gw#s_input,gw#set_disabled

let make_dhcp () =
  let dhcp  = new Switch.t () in
  let form  = new Form_field.t ~input:dhcp ~label:"DHCP" ~align_end:true () in
  let set (x:nw) = dhcp#set_checked x.dhcp in
  form#widget,set,dhcp#s_state,dhcp#set_disabled

let name     = "Настройки. Сеть"
let settings = None

let make ~(state: Topology.state React.signal)
         ~(mode:  nw React.signal)
         (_:   config option)
         control =
  let ip,set_ip,s_ip,dis_ip         = make_ip () in
  let mask,set_mask,s_mask,dis_mask = make_mask () in
  let gw,set_gw,s_gw,dis_gw         = make_gateway () in
  let dhcp,set_dhcp,s_dhcp,dis_dhcp = make_dhcp () in
  let s : nw option React.signal =
    React.S.l5 (fun ip mask gw dhcp state ->
        match ip,mask,gw,state with
        | Some ip,Some mask,Some gw,`Fine -> Some { ip;mask;gateway=gw;dhcp }
        | _                                         -> None)
               s_ip s_mask s_gw s_dhcp state
  in
  let _ = React.S.map (fun x -> List.iter (fun f -> f x) [set_dhcp;set_ip;set_mask;set_gw]) mode in
  let _ = React.S.l2 (fun state dhcp ->
              let is_disabled = match state,dhcp with
                | `Fine,false -> false
                | _           -> true
              in
              List.iter (fun f -> f is_disabled) [dis_ip;dis_mask;dis_gw];
              dis_dhcp (match state with `Fine -> false | _ -> true);
            ) state s_dhcp
  in
  let submit = fun x -> Requests.Device.HTTP.set_mode x control in
  let apply  = Ui_templates.Buttons.create_apply s submit in
  let box    = new Box.t ~vertical:true ~widgets:[dhcp;ip;mask;gw;apply#widget] () in
  let ()     = box#add_class "mdc-settings-widget" in
  box#widget
