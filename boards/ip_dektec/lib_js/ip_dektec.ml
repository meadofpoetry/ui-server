open Components
open Ui

type strings =
  { apply : string
  }

let strings =
  { apply = "применить"
  }

let format_bitrate x =
  let gbit = 1_000_000_000. in
  let mbit = 1_000_000. in
  let kbit = 1_000. in
  let v,s = (match (float_of_int x) with
             | x when x >= gbit -> x /. gbit, "Гбит/с"
             | x when x >= mbit -> x /. mbit, "Мбит/с"
             | x when x >= kbit -> x /. kbit, "Кбит/с"
             | x -> x, "Bit/s") in
  Printf.sprintf "%.2f %s" v s

let main_status_card s_state e_status =
  let open Board_types in
  let rows     =
    [ new Row.t ~label:"TP per IP" ~e:(React.E.map (fun s -> string_of_int s.tp_per_ip) e_status) ()
    ; new Row.t ~label:"Протокол" ~e:(React.E.map (fun s -> protocol_to_string s.protocol) e_status) ()
    ; new Row.t ~label:"Размер пакета" ~e:(React.E.map (fun s -> packet_sz_to_string s.packet_size) e_status) ()
    ; new Row.t ~label:"Входной битрейт" ~e:(React.E.map (fun s -> format_bitrate s.bitrate) e_status) ()
    ; new Row.t ~label:"Выходной битрейт" ~e:(React.E.map (fun s -> format_bitrate s.bitrate) e_status) ()
    ; new Row.t
          ~icon:true
          ~label:"Наличие PCR"
          ~e:(React.E.map (fun s -> if s.pcr_present then "check" else "close") e_status)
          ()
    ] in
  let params = new Rows.t ~s_state ~rows () in
  let media  = new Card.Media.t ~widgets:[params] () in
  new Stateful_card.t ~title:"Статус" ~s_state ~sections:[`Media media ] ()

let fec_status_card s_state e_status =
  let open Board_types in
  let rows =
    [ new Row.t ~label:"Задержка FEC"     ~e:(React.E.map (fun s -> string_of_int s.fec_delay) e_status) ()
    ; new Row.t ~label:"FEC столбцов"     ~e:(React.E.map (fun s -> string_of_int s.fec_cols) e_status) ()
    ; new Row.t ~label:"FEC строк"        ~e:(React.E.map (fun s -> string_of_int s.fec_rows) e_status) ()
    ; new Row.t ~label:"Потерь перед FEC" ~e:(React.E.map (fun s -> Int64.to_string s.lost_before_fec) e_status) ()
    ; new Row.t ~label:"Потерь после FEC" ~e:(React.E.map (fun s -> Int64.to_string s.lost_after_fec) e_status) ()
    ] in
  let params = new Rows.t ~s_state ~rows () in
  let media  = new Card.Media.t ~widgets:[params] () in
  new Stateful_card.t ~title:"Статус FEC" ~sections:[ `Media media ] ~s_state ()

let fec_chart s_state e_status =
  let open Board_types in
  let open Chartjs.Line in
  let config = new Config.t
                   ~x_axis:(Time ("my-x-axis",Bottom,Unix,Some 60000L))
                   ~y_axis:(Linear ("my-y-axis",Left,Integer,None))
                   ~data:[ { data = []; label = "Lost before FEC" }
                         ; { data = []; label = "Lost after FEC"} ]
                   () in
  config#options#hover#set_mode Index;
  config#options#hover#set_intersect true;
  config#options#tooltip#set_mode Index;
  config#options#tooltip#set_intersect false;
  config#options#x_axis#time#set_min_unit Second;
  config#options#y_axis#ticks#set_begin_at_zero true;
  List.iter (fun x -> if x#get_label = "Lost before FEC"
                      then x#set_background_color @@ Color.rgb_of_name (Color.Indigo C500)
                      else x#set_background_color @@ Color.rgb_of_name (Color.Amber C500);
                      if x#get_label = "Lost before FEC"
                      then x#set_border_color @@ Color.rgb_of_name (Color.Indigo C500)
                      else x#set_border_color @@ Color.rgb_of_name (Color.Amber C500);
                      x#set_cubic_interpolation_mode Monotone;
                      x#set_fill Disabled) config#datasets;
  let chart = new t ~config () in
  let before_ds = CCList.find (fun x -> x#get_label = "Lost before FEC") chart#config#datasets in
  let after_ds  = CCList.find (fun x -> x#get_label = "Lost after FEC") chart#config#datasets in
  React.E.map (fun s -> let now = Int64.of_float @@ Unix.time () *. 1000. in
                        before_ds#push { x = now; y = Int64.to_int s.lost_before_fec };
                        after_ds#push { x = now; y = Int64.to_int s.lost_after_fec };
                        chart#update None)
              e_status |> ignore;
  chart


let fec_bar_chart s_state e_status =
  let open Board_types in
  let open Chartjs.Bar in
  let config = new Config.t
                   ~x_axis:(Time ("my-x-axis",Bottom,Unix,Some 60000L))
                   ~y_axis:(Linear ("my-y-axis",Left,Integer,None))
                   ~data:[ { data = []; label = "Lost before FEC"; stack = None }
                         ; { data = []; label = "Lost after FEC" ; stack = None } ]
                   () in
  config#options#hover#set_mode Index;
  config#options#hover#set_intersect true;
  config#options#tooltip#set_mode Index;
  config#options#tooltip#set_intersect false;
  config#options#x_axis#time#set_min_unit Second;
  config#options#y_axis#ticks#set_begin_at_zero true;
  config#options#x_axis#set_bar_percentage 1.;
  config#options#x_axis#set_category_percentage 0.5;
  config#options#x_axis#ticks#set_source Data;
  List.iter (fun x -> if x#get_label = "Lost before FEC"
                      then x#set_background_color @@ `Val (Color.rgb_of_name (Color.Indigo C500))
                      else x#set_background_color @@ `Val (Color.rgb_of_name (Color.Amber C500));
                      if x#get_label = "Lost before FEC"
                      then x#set_border_color @@ `Val (Color.rgb_of_name (Color.Indigo C500))
                      else x#set_border_color @@ `Val (Color.rgb_of_name (Color.Amber C500));) config#datasets;
  let chart = new t ~config () in
  let before_ds = CCList.find (fun x -> x#get_label = "Lost before FEC") chart#config#datasets in
  let after_ds  = CCList.find (fun x -> x#get_label = "Lost after FEC") chart#config#datasets in
  React.E.map (fun s -> let now = Int64.of_float @@ Unix.time () *. 1000. in
                        before_ds#push { x = now; y = Int64.to_int s.lost_before_fec };
                        after_ds#push { x = now; y = Int64.to_int s.lost_after_fec };
                        chart#update None)
              e_status |> ignore;
  chart

let bitrate_chart s_state e_status =
  let open Board_types in
  let open Chartjs.Line in
  let config = new Config.t
                   ~x_axis:(Time ("my-x-axis",Bottom,Unix,Some 60000L))
                   ~y_axis:(Linear ("my-y-axis",Left,Float,None))
                   ~data:[ { data = []; label = "Input bitrate" }
                         ; { data = []; label = "Output bitrate"} ]
                   () in
  config#options#hover#set_mode Index;
  config#options#hover#set_intersect true;
  config#options#tooltip#set_mode Index;
  config#options#tooltip#set_intersect false;
  config#options#x_axis#time#set_min_unit Second;
  config#options#y_axis#scale_label#set_display true;
  config#options#y_axis#scale_label#set_label_string "Mbit/s";
  config#options#y_axis#ticks#set_begin_at_zero true;
  List.iter (fun x -> if x#get_label = "Input bitrate"
                      then x#set_background_color @@ Color.rgb_of_name (Color.Indigo C500)
                      else x#set_background_color @@ Color.rgb_of_name (Color.Amber C500);
                      if x#get_label = "Input bitrate"
                      then x#set_border_color @@ Color.rgb_of_name (Color.Indigo C500)
                      else x#set_border_color @@ Color.rgb_of_name (Color.Amber C500);
                      x#set_cubic_interpolation_mode Monotone;
                      x#set_fill Disabled) config#datasets;
  let chart = new t ~config () in
  let input_ds  = CCList.find (fun x -> x#get_label = "Input bitrate") chart#config#datasets in
  let output_ds = CCList.find (fun x -> x#get_label = "Output bitrate") chart#config#datasets in
  React.E.map (fun s -> let now = Int64.of_float @@ Unix.time () *. 1000. in
                        input_ds#push { x = now; y = (float_of_int s.bitrate) /. 1000000.};
                        output_ds#push { x = now; y = (float_of_int s.asi_bitrate) /. 1000000. };
                        chart#update None)
              e_status |> ignore;
  chart

let charts s_state e_status =
  new Box.t ~widgets:[ (new Box.t
                            ~vertical:false
                            ~widgets:[(fec_chart s_state e_status)#widget
                                     ; (fec_bar_chart s_state e_status)#widget] ())#widget
                     ; (bitrate_chart s_state e_status)#widget ] ()

let nw_settings_block s_state (cfg:Board_types.config) =
  let help_text : Textfield.Help_text.helptext = { validation=true;persistent=false;text=None } in
  let ip        = new Textfield.t ~input_type:IPV4 ~help_text ~label:"IP адрес" () in
  let mask      = new Textfield.t ~input_type:IPV4 ~help_text ~label:"Маска подсети" () in
  let gw        = new Textfield.t ~input_type:IPV4 ~help_text ~label:"Шлюз" () in
  let dhcp      = new Switch.t ~input_id:"dhcp" () in
  let settings  = new Box.t
                      ~vertical:true
                      ~widgets:[ (new Form_field.t ~input:dhcp ~label:"DHCP" ~align_end:true ())#widget
                               ; ip#widget
                               ; mask#widget
                               ; gw#widget ]
                      () in
  let media     = new Card.Media.t ~widgets:[settings] () in
  ip#set_required true; mask#set_required true; gw#set_required true;
  (* ip#set_ip_pattern; mask#set_ip_pattern; gw#set_ip_pattern; *)
  ip#set_value   @@ Ipaddr.V4.to_string cfg.nw.ip;
  mask#set_value @@ Ipaddr.V4.to_string cfg.nw.mask;
  gw#set_value   @@ Ipaddr.V4.to_string cfg.nw.gateway;
  dhcp#set_checked cfg.nw.dhcp;
  React.S.map (fun x -> ip#set_disabled x; mask#set_disabled x; gw#set_disabled x) dhcp#s_state |> ignore;
  React.S.map (fun x -> dhcp#set_disabled @@ not x;
                        ip#set_disabled @@ not x;
                        mask#set_disabled @@ not x;
                        gw#set_disabled @@ not x) s_state |> ignore;
  new Settings_card.t
      ~s_state
      ~f_submit:(fun () ->
        let open Lwt_result.Infix in
        Requests.post_dhcp 4 dhcp#get_checked
        >>= (fun _ -> Requests.post_address 4 @@ Ipaddr.V4.of_string_exn ip#get_value)
        >>= (fun _ -> Requests.post_mask 4    @@ Ipaddr.V4.of_string_exn mask#get_value)
        >>= (fun _ -> Requests.post_gateway 4 @@ Ipaddr.V4.of_string_exn gw#get_value)
        >>= (fun _ -> Requests.post_reset 4))
      ~s_valid:(React.S.const true)(* (React.S.merge (fun a s -> a && s) true [ip#s_valid;mask#s_valid;gw#s_valid]) *)
      ~title:"Сетевые настройки"
      ~sections:[ `Media media ]
      ()

let ip_settings_block s_state (cfg:Board_types.config) =
  let help_text : Textfield.Help_text.helptext = { validation=true;persistent=false;text=None } in
  let en        = new Switch.t ~input_id:"enable" () in
  let fec       = new Switch.t ~input_id:"fec" () in
  let mcast_en  = new Switch.t ~input_id:"mcast_en" () in
  let port      = new Textfield.t ~help_text ~label:"UDP порт" ~input_type:(Integer (Some (0,65535))) () in
  let multicast = new Textfield.t ~help_text ~label:"Multicast адрес" ~input_type:MulticastV4 () in
  let widgets   = [ Widget.coerce @@ new Form_field.t ~label:"Включить приём TSoIP" ~align_end:true ~input:en ()
                  ; Widget.coerce @@ new Form_field.t ~label:"Включить FEC" ~align_end:true ~input:fec ()
                  ; Widget.coerce @@ new Form_field.t ~label:"Включить Multicast" ~align_end:true ~input:mcast_en ()
                  ; Widget.coerce multicast
                  ; Widget.coerce port] in
  let media     = new Card.Media.t ~widgets:[ new Box.t ~vertical:true ~widgets () ] () in
  mcast_en#set_checked @@ CCOpt.is_some cfg.ip.multicast;
  (* multicast#set_multicast_pattern; *)
  multicast#set_required true;
  port#set_min 0.0;
  port#set_max 65535.0;
  port#set_required true;
  en#set_checked  cfg.ip.enable;
  fec#set_checked cfg.ip.fec;
  port#set_value @@ string_of_int cfg.ip.port;
  CCOpt.iter (fun x -> multicast#set_value @@ Ipaddr.V4.to_string x) cfg.ip.multicast;
  React.S.map (fun x -> multicast#set_disabled @@ not x) mcast_en#s_state |> ignore;
  React.S.map (fun x -> en#set_disabled @@ not x;
                        fec#set_disabled @@ not x;
                        mcast_en#set_disabled @@ not x;
                        port#set_disabled @@ not x;
                        multicast#set_disabled @@ not x) s_state |> ignore;
  new Settings_card.t
      ~s_state
      ~f_submit:(fun () ->
        let open Lwt_result.Infix in
        Requests.post_ip_enable 4 en#get_checked
        >>= (fun _ -> Requests.post_fec 4 fec#get_checked)
        >>= (fun _ -> Requests.post_port 4 @@ int_of_string @@ port#get_value)
        >>= (fun _ -> Requests.post_meth 4 @@ if mcast_en#get_checked then Multicast else Unicast)
        >>= (fun _ -> Requests.post_multicast 4 @@ Ipaddr.V4.of_string_exn multicast#get_value))
      ~s_valid:(React.S.const true) (* (React.S.merge (fun a s -> a && s) true [port#s_valid; multicast#s_valid]) *)
      ~title:"Настройки приёма TSoIP"
      ~sections:[ `Media media ]
      ()

let qos () =
  let items = [ `Item (new Select.Pure.Item.t ~text:"ASI" ())
              ; `Item (new Select.Pure.Item.t ~text:"SPI" ())
              ] in
  let select = new Select.Pure.t ~items () in
  Dom_events.listen select#select_element
                    Dom_events.Typ.change
                    (fun _ _ -> let open Board_qos_niit_js in
                                let open Board_qos_niit_js.Board_types in
                                (match select#get_selected_index with
                                 | Some 0 -> Requests.post_mode 2 { input = ASI
                                                                  ; t2mi  = None } |> ignore;
                                 | Some 1 -> Requests.post_mode 2 { input = SPI
                                                                  ; t2mi  = None } |> ignore;
                                 | _   -> ());
                                false)
  |> ignore;
  select

let load () =
  let open Lwt_result.Infix in
  Requests.get_config 4
  >>= (fun cfg ->
    Requests.get_state 4
    >>= (fun state ->
         let s_state     = React.S.map (function
                                        | `No_response | `Init -> false
                                        | `Fine -> true) @@ React.S.hold state @@ Requests.get_state_socket 4 in
         let e_status    = Requests.get_status_socket 4 in
         let container   = Dom_html.getElementById "ip_widgets" in

         let status_card = main_status_card s_state e_status in
         let fec_card    = fec_status_card s_state e_status in
         let nw_card     = nw_settings_block s_state cfg in
         let ip_card     = ip_settings_block s_state cfg in

         let status_grid   = new Layout_grid.t ~cells:[ new Layout_grid.Cell.t ~widgets:[ status_card ] ()
                                                      ; new Layout_grid.Cell.t ~widgets:[ fec_card ] ()
                                                      ] () in
         let settings_grid = new Layout_grid.t ~cells:[ new Layout_grid.Cell.t ~widgets:[ nw_card ] ()
                                                      ; new Layout_grid.Cell.t ~widgets:[ ip_card ] ()
                                                      ] () in
         let charts_grid = charts s_state e_status in
         charts_grid#style##.maxWidth := Js.string "700px";
         Dom.appendChild container (qos ())#root;
         Dom.appendChild container settings_grid#root;
         Dom.appendChild container status_grid#root;
         Dom.appendChild container charts_grid#root;
         Lwt_result.return ()))
  |> ignore
