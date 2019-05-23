open Application_types
open Board_dektec_dtm3200_types
open Board_dektec_dtm3200_http_js
open Components

let base_class = "ip-dektec-receiver-settings"

let make_enable () =
  let en   = new Switch.t () in
  let form =
    new Form_field.t
      ~input:en
      ~label:"Включить приём TSoIP"
      ~align_end:true () in
  let set (x : ip_receive) = en#set_checked x.enable in
  form#widget, set, en#s_state, en#set_disabled

let make_fec () =
  let en = new Switch.t () in
  let form =
    new Form_field.t
      ~input:en
      ~label:"Включить FEC"
      ~align_end:true () in
  let set (x : ip_receive) = en#set_checked x.fec_enable in
  form#widget, set, en#s_state, en#set_disabled

let make_meth () =
  let en   = new Switch.t () in
  let form =
    new Form_field.t
      ~input:en
      ~label:"Включить Multicast"
      ~align_end:true () in
  let set (x : ip_receive) = match x.addressing_method with
    | Unicast -> en#set_checked false
    | Multicast -> en#set_checked true in
  form#widget, set, en#s_state, en#set_disabled

let make_port () =
  let port =
    new Textfield.t
      ~label:"UDP порт"
      ~input_type:(Integer (Some 0, Some 65535)) () in
  let set (x : ip_receive) = port#set_value x.udp_port in
  port#widget, set, port#s_input, port#set_disabled

let make_multicast () =
  let mcast =
    new Textfield.t
      ~input_id:"mcast"
      ~label:"Multicast адрес"
      ~input_type:MulticastV4 () in
  let set (x : ip_receive) = mcast#set_value x.multicast in
  mcast#widget, set, mcast#s_input, mcast#set_disabled

let name = "Настройки. Приём"
let settings = None

let ( >>= ) = Lwt_result.( >>= )

(* FIXME declare class instead *)
let make ~(state : Topology.state React.signal)
    ~(mode : ip_receive React.signal)
    control =
  let en, set_en, s_en, dis_en = make_enable () in
  let fec, set_fec, s_fec, dis_fec = make_fec () in
  let meth, set_meth, s_meth, dis_meth = make_meth () in
  let port, set_port, s_port, dis_port = make_port () in
  let mcast, set_mcast, s_mcast, dis_mcast = make_multicast () in
  let (s : ip_receive option React.signal) =
    React.S.l6 ~eq:(Util_equal.Option.equal equal_ip_receive)
      (fun en fec_enable meth udp_port multicast state ->
         let ip_to_output_delay = (React.S.value mode).ip_to_output_delay in
         let rate_mode = (React.S.value mode).rate_mode in
         match udp_port, multicast, state with
         | Some udp_port, Some multicast, `Fine ->
           Some ({ enable = en
                 ; fec_enable
                 ; multicast
                 ; ip_to_output_delay
                 ; addressing_method = (if meth then Multicast else Unicast)
                 ; udp_port
                 ; rate_mode
                 } : ip_receive)
         | _ -> None)
      s_en s_fec s_meth s_port s_mcast state in
  let s_dis =
    React.S.map ~eq:(=) (fun state ->
        let is_disabled = match state with
          | `Fine -> false
          | _     -> true in
        List.iter (fun f -> f is_disabled) [dis_en;dis_fec;dis_meth;dis_port])
      state in
  let s_set =
    React.S.map ~eq:(=) (fun (ip : ip_receive) ->
        let setters = [ set_en; set_fec; set_meth; set_port; set_mcast ] in
        List.iter (fun f -> f ip) setters) mode in
  let submit (x : ip_receive) = Http_receiver.(
      set_enable x.enable control
      >>= fun _ -> set_fec_enable x.fec_enable control
      >>= fun _ -> set_multicast_address x.multicast control
      >>= fun _ -> set_ip_to_output_delay x.ip_to_output_delay control
      >>= fun _ -> set_addressing_method x.addressing_method control
      >>= fun _ -> set_udp_port x.udp_port control
      >>= fun _ -> set_rate_estimation_mode x.rate_mode control
      >>= fun _ -> Lwt.return_ok ()) in
  let apply = new Ui_templates.Buttons.Set.t s submit () in
  let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
  let actions = new Card.Actions.t ~widgets:[buttons] () in
  let widgets = [en; fec; meth; mcast; port; actions#widget] in
  let box = new Vbox.t ~widgets () in
  box#add_class base_class;
  box#set_on_destroy (fun () ->
      React.S.stop ~strong:true s_dis;
      React.S.stop ~strong:true s_set);
  box#widget
