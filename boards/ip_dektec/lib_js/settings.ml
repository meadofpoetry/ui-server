open Containers
open Components
open Board_types
open Boards_js.Types

let make_nw_settings ~(init:  nw)
                     ~(event: nw React.event)
                     ~(state: Common.Topology.state React.signal)
                     control
                     () : (nw,unit) settings_block =
  let ht : Textfield.Help_text.helptext = { validation=true;persistent=false;text=None} in
  let ip   = new Textfield.t ~input_id:"ip"   ~input_type:IPV4 ~help_text:ht ~label:"IP адрес" () in
  let mask = new Textfield.t ~input_id:"mask" ~input_type:IPV4 ~help_text:ht ~label:"Маска подсети" () in
  let gw   = new Textfield.t ~input_id:"gw"   ~input_type:IPV4 ~help_text:ht ~label:"Шлюз" () in
  let dhcp = new Switch.t ~input_id:"dhcp" () in
  let box  = new Box.t ~vertical:true
                 ~widgets:[ (new Form_field.t ~input:dhcp ~label:"DHCP" ~align_end:true ())#widget
                          ; ip#widget
                          ; mask#widget
                          ; gw#widget
                          ]
                 ()
  in
  let inputs = [ip;mask;gw] in
  let ()     = List.iter (fun x -> x#set_required true) inputs in
  let _      = React.S.map (fun x -> List.iter (fun i -> i#set_disabled x) inputs) dhcp#s_state in
  let _      = React.S.map (fun state ->
                   let is_disabled = match state with
                     | `Fine -> false
                     | _     -> true
                   in
                   dhcp#set_disabled is_disabled;
                   List.iter (fun x -> x#set_disabled is_disabled) inputs) state
  in
  let _      = React.S.map (fun (nw:nw) -> let data   = [nw.ip;nw.mask;nw.gateway] in
                                           let ()     = List.iter2 (fun i v -> i#fill_in v) inputs data in
                                           let ()     = dhcp#set_checked nw.dhcp in
                                           ())
               @@ React.S.hold ~eq:Board_types.equal_nw init event
  in
  let s      = React.S.l5 (fun dhcp ip mask gw state -> match ip,mask,gw,state with
                                                        | Some ip,Some mask,Some gw,`Fine ->
                                                           Some { ip;mask;gateway=gw;dhcp }
                                                        | _ -> None)
                          dhcp#s_state ip#s_input mask#s_input gw#s_input state
  in
  let submit = fun (cfg:nw) -> let open Lwt_result.Infix in
                               Requests.post_dhcp control cfg.dhcp
                               >>= (fun _ -> Requests.post_address control cfg.ip)
                               >>= (fun _ -> Requests.post_mask    control cfg.mask)
                               >>= (fun _ -> Requests.post_gateway control cfg.gateway)
                               >>= (fun _ -> Requests.post_reset control)
  in
  box#widget,s,submit

let make_ip_settings ~(init:  ip)
                     ~(event: ip React.event)
                     ~(state: Common.Topology.state React.signal)
                     control
                     () : (ip,unit) settings_block =
  let ht : Textfield.Help_text.helptext = { validation=true;persistent=false;text=None } in
  let en       = new Switch.t ~input_id:"enable" () in
  let fec      = new Switch.t ~input_id:"fec" () in
  let mcast_en = new Switch.t ~input_id:"mcast_en" () in
  let port     = new Textfield.t ~input_id:"port" ~help_text:ht ~label:"UPD порт"
                     ~input_type:(Integer (Some 0, Some 65535)) ()
  in
  let mcast    = new Textfield.t ~input_id:"mcast" ~help_text:ht ~label:"Multicast адрес"
                     ~input_type:MulticastV4 ()
  in
  let box      =
    new Box.t ~vertical:true
        ~widgets:[ (new Form_field.t ~label:"Включить приём TSoIP" ~align_end:true ~input:en ())#widget
                 ; (new Form_field.t ~label:"Включить FEC" ~align_end:true ~input:fec ())#widget
                 ; (new Form_field.t ~label:"Включить Multicast" ~align_end:true ~input:mcast_en ())#widget
                 ; mcast#widget
                 ; port#widget
                 ]
        ()
  in
  let switches = [en;fec;mcast_en] in
  let () = mcast#set_required true in
  let () = port#set_required true in
  let _  = React.S.l2 (fun mcast_en state ->
               let is_disabled = match state with
                 | `Fine -> false
                 | _     -> true
               in
               port#set_disabled is_disabled;
               if is_disabled
               then mcast#set_disabled true
               else mcast#set_disabled @@ not mcast_en;
               List.iter (fun x -> x#set_disabled is_disabled) switches) mcast_en#s_state state
  in
  let _  = React.S.map (fun (ip:ip) -> en#set_checked ip.enable;
                                       fec#set_checked ip.fec;
                                       mcast_en#set_checked @@ Option.is_some ip.multicast;
                                       port#fill_in ip.port;
                                       Option.iter (fun x -> mcast#fill_in x) ip.multicast)
           @@ React.S.hold ~eq:equal_ip init event
  in
  let s  = React.S.l6 (fun en fec mcast_en port mcast state ->
               match port,mcast_en,mcast,state with
               | Some port,true,Some mcast,`Fine -> Some ({ enable = en
                                                          ; fec    = fec
                                                          ; multicast = Some mcast
                                                          ; delay     = init.delay
                                                          ; port
                                                          ; rate_mode = init.rate_mode
                                                          } : ip)
               | Some port,false,None,`Fine      -> Some ({ enable = en
                                                          ; fec = fec
                                                          ; multicast = None
                                                          ; delay = init.delay
                                                          ; port
                                                          ; rate_mode = init.rate_mode
                                                          } : ip)
               | _ -> None)
                      en#s_state fec#s_state mcast_en#s_state port#s_input mcast#s_input state
  in
  let submit = fun (ip:ip) ->
    let open Lwt_result.Infix in
    Requests.post_ip_enable control ip.enable
    >>= (fun _ -> Requests.post_fec control ip.fec)
    >>= (fun _ -> Requests.post_port control ip.port)
    >>= (fun _ -> Requests.post_meth control @@ if Option.is_some ip.multicast then Multicast else Unicast)
    >>= (fun _ -> match ip.multicast with
                  | Some m -> Requests.post_multicast control m >>= (fun _ -> Lwt_result.return ())
                  | None   -> Lwt_result.return ())
  in
  box#widget,s,submit
