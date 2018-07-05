open Board_types
open Containers
open Components
open Common

type config = unit [@@deriving yojson]

let make_enable () =
  let en   = new Switch.t () in
  let form = new Form_field.t ~input:en ~label:"Включить приём TSoIP" ~align_end:true () in
  let set (x:ip) = en#set_checked x.enable in
  form#widget,set,en#s_state,en#set_disabled

let make_fec () =
  let en   = new Switch.t () in
  let form = new Form_field.t ~input:en ~label:"Включить FEC" ~align_end:true () in
  let set (x:ip) = en#set_checked x.enable in
  form#widget,set,en#s_state,en#set_disabled

let make_meth () =
  let en   = new Switch.t () in
  let form = new Form_field.t ~input:en ~label:"Включить Multicast" ~align_end:true () in
  let set (x:ip) = en#set_checked @@ Option.is_some x.multicast in
  form#widget,set,en#s_state,en#set_disabled

let make_port () =
  let help_text : Textfield.Help_text.helptext = { validation=true;persistent=false;text=None } in
  let port = new Textfield.t ~input_id:"port" ~help_text ~label:"UDP порт"
                 ~input_type:(Integer (Some 0, Some 65535)) ()
  in
  let set (x:ip) = port#fill_in x.port in
  port#widget,set,port#s_input,port#set_disabled

let make_multicast () =
  let help_text : Textfield.Help_text.helptext = { validation=true;persistent=false;text=None } in
  let mcast = new Textfield.t ~input_id:"mcast" ~help_text ~label:"Multicast адрес"
                  ~input_type:MulticastV4 ()
  in
  let set (x:ip) = match x.multicast with
    | None   -> ()
    | Some x -> mcast#fill_in x
  in
  mcast#widget,set,mcast#s_input,mcast#set_disabled

let name     = "Настройки. Приём"
let settings = None

let make ~(state: Topology.state React.signal)
         ~(mode:  ip React.signal)
         (_:      config option)
         control =
  let en,set_en,s_en,dis_en             = make_enable () in
  let fec,set_fec,s_fec,dis_fec         = make_fec () in
  let meth,set_meth,s_meth,dis_meth     = make_meth () in
  let port,set_port,s_port,dis_port     = make_port () in
  let mcast,set_mcast,s_mcast,dis_mcast = make_multicast () in
  let s : ip option React.signal =
    React.S.l6 (fun en fec meth port mcast state ->
        let delay     = (React.S.value mode).delay in
        let rate_mode = (React.S.value mode).rate_mode in
        match port,meth,mcast,state with
        | Some port,true,Some mcast,`Fine ->
           Some ({ enable = en; fec; multicast = Some mcast; delay; port; rate_mode } : ip)
        | Some port,false,_,`Fine      ->
           Some ({ enable = en; fec; multicast = None; delay; port; rate_mode } : ip)
        | _ -> None)
               s_en s_fec s_meth s_port s_mcast state
  in
  let _  = React.S.l2 (fun mcast_en state ->
               let is_disabled = match state with
                 | `Fine -> false
                 | _     -> true
               in
               List.iter (fun f -> f is_disabled) [dis_en;dis_fec;dis_meth;dis_port];
               if is_disabled
               then dis_mcast true
               else dis_mcast @@ not mcast_en) s_meth state
  in
  let _ = React.S.map (fun (ip:ip) ->
              List.iter (fun f -> f ip) [set_en;set_fec;set_meth;set_port;set_mcast]) mode
  in
  let submit = fun x -> Requests.Receiver.HTTP.set_mode x control in
  let apply  = Ui_templates.Buttons.create_apply s submit in
  let box    = new Vbox.t ~widgets:[en;fec;meth;mcast;port;apply#widget] () in
  let ()     = box#add_class "mdc-settings-widget" in
  box#widget
