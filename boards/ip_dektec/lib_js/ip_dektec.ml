open Components

let mcast_regexp = "2(?:2[4-9]|3\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]\\d?|0)){3}"
let ip_regexp    = "^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"

module Row = struct

  open Tyxml_js.Html

  class t ?(icon=false) ~label ~e () =
    let nw = span [pcdata label] |> Tyxml_js.To_dom.of_element |> Widget.create in
    let vw = if icon
             then new Icon.Font.t ~icon:"" () |> Widget.coerce
             else span [pcdata "-"] |> Tyxml_js.To_dom.of_element |> Widget.create in
    object
      inherit Box.t ~widgets:[Widget.coerce nw; Widget.coerce vw] () as super
      method has_icon = icon
      method get_value_widget = vw
      method get_label_widget = nw
      initializer
        Typography.set ~font:Subheading_1 nw;
        Typography.set ~font:Body_2 vw;
        React.E.map (fun s -> vw#set_text_content s) e |> ignore;
        super#set_justify_content `Space_between
    end

end

module Rows = struct

  class t ~(rows:Row.t list) ~e_state () = object
    inherit Box.t ~widgets:rows () as super
    initializer
      React.E.map (function
                   | `No_response | `Init -> List.iter (fun row -> let s = if row#has_icon then "" else "-" in
                                                                   row#get_value_widget#set_text_content s)
                                                       rows
                   | `Fine -> ()) e_state |> ignore;
      super#set_vertical
  end

end

module Stateful_card = struct

  module Primary = struct

    class t ?subtitle ~title ~e_state () =
      let title_widget    = new Card.Title.t ~title () in
      let subtitle_widget = CCOpt.map (fun x -> new Card.Subtitle.t ~subtitle:x ()) subtitle in
    object
      inherit Card.Primary.t ~widgets:([]
                                       |> CCList.cons_maybe @@ CCOpt.map Widget.coerce subtitle_widget
                                       |> CCList.cons @@ Widget.coerce title_widget)
                             () as super
      initializer
        title_widget#add_class "color-primary-on-primary";
        super#add_class "background-primary";
    end

  end

  class t ?subtitle ~title ~sections ~e_state () = object
    inherit Card.t ~sections:(CCList.cons (`Primary (new Primary.t ?subtitle ~title ~e_state ()))
                                          sections)
                   () as super
    initializer
      super#style##.height := Js.string "100%";
      CCOpt.iter (fun x -> x#style##.height := Js.string "100%";
                           (Js.Unsafe.coerce x#style)##.justifyContent := Js.string "flex-start") super#get_media;
      React.E.map (function
                   | `No_response | `Init -> super#style##.backgroundColor := Js.string "#ef9a9a"
                   | `Fine -> super#style##.backgroundColor := Js.string "") e_state |> ignore
  end

end

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

let status_row name e f =
  let open Tyxml_js.Html in
  let nw = span [pcdata name] |> Tyxml_js.To_dom.of_element |> Widget.create in
  let vw = span [pcdata "-"]  |> Tyxml_js.To_dom.of_element |> Widget.create in
  Typography.set ~font:Subheading_1 nw;
  Typography.set ~font:Body_2 vw;
  React.E.map (fun s -> vw#set_text_content @@ f s) e |> ignore;
  let box = new Box.t ~widgets:[nw; vw] () in
  box#set_justify_content `Space_between;
  box

let icon_row name e f =
  let open Tyxml_js.Html in
  let nw = span [pcdata name] |> Tyxml_js.To_dom.of_element |> Widget.create in
  let vw = new Icon.Font.t ~icon:"" () in
  Typography.set ~font:Subheading_1 nw;
  Typography.set ~font:Body_2 vw;
  React.E.map (fun s -> vw#set_icon @@ f s) e |> ignore;
  let box = new Box.t ~widgets:[Widget.coerce nw; Widget.coerce vw] () in
  box#set_justify_content `Space_between;
  box

let main_status_card e_state e_status =
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
  let params = new Rows.t ~e_state ~rows () in
  let media  = new Card.Media.t ~widgets:[params] () in
  new Stateful_card.t ~title:"Статус" ~e_state ~sections:[`Media media ] ()

let fec_status_card e_state e_status =
  let open Board_types in
  let rows =
    [ new Row.t ~label:"Задержка FEC"     ~e:(React.E.map (fun s -> string_of_int s.fec_delay) e_status) ()
    ; new Row.t ~label:"FEC столбцов"     ~e:(React.E.map (fun s -> string_of_int s.fec_cols) e_status) ()
    ; new Row.t ~label:"FEC строк"        ~e:(React.E.map (fun s -> string_of_int s.fec_rows) e_status) ()
    ; new Row.t ~label:"Потерь перед FEC" ~e:(React.E.map (fun s -> Int64.to_string s.lost_before_fec) e_status) ()
    ; new Row.t ~label:"Потерь после FEC" ~e:(React.E.map (fun s -> Int64.to_string s.lost_after_fec) e_status) ()
    ] in
  let params = new Rows.t ~e_state ~rows () in
  let media  = new Card.Media.t ~widgets:[params] () in
  new Stateful_card.t ~title:"Статус FEC" ~sections:[ `Media media ] ~e_state ()

let nw_settings_block e_state (cfg:Board_types.config) =
  let ip        = new Textfield.t ~label:"IP адрес" () in
  let mask      = new Textfield.t ~label:"Маска подсети" () in
  let gw        = new Textfield.t ~label:"Шлюз" () in
  let dhcp_sw   = new Switch.t ~input_id:"dhcp" () in
  let dhcp_form = new Form_field.t ~label:"DHCP" ~input:dhcp_sw ~align_end:true () in
  let settings  = new Box.t
                      ~widgets:[ Widget.coerce dhcp_form
                               ; Widget.coerce ip
                               ; Widget.coerce mask
                               ; Widget.coerce gw ]
                      () in
  let media     = new Card.Media.t ~widgets:[settings] () in
  let apply_btn = new Button.t ~label:strings.apply () in
  let actions   = new Card.Actions.t ~widgets:[apply_btn] () in
  (* Applying styles *)
  apply_btn#set_raised false;
  apply_btn#set_compact true;
  settings#set_vertical;
  dhcp_form#style##.marginBottom := Js.string "16px";
  (* Applying validation rules *)
  ip#set_pattern ip_regexp;
  mask#set_pattern ip_regexp;
  gw#set_pattern ip_regexp;
  (* Inserting values *)
  ip#set_value   @@ Ipaddr.V4.to_string cfg.nw.ip;
  mask#set_value @@ Ipaddr.V4.to_string cfg.nw.mask;
  gw#set_value   @@ Ipaddr.V4.to_string cfg.nw.gateway;
  dhcp_sw#set_checked cfg.nw.dhcp;
  (* Reactify *)
  React.S.map (fun x -> ip#set_disabled x; mask#set_disabled x; gw#set_disabled x) dhcp_sw#s_state |> ignore;
  React.E.map (function
               | `No_response | `Init -> apply_btn#set_disabled true
               | _ -> apply_btn#set_disabled false) e_state |> ignore;
  (* Listening apply btn *)
  React.E.map (fun () -> apply_btn#set_disabled true;
                         let open Lwt_result.Infix in
                         Requests.post_dhcp 4 dhcp_sw#get_checked
                         >>= (fun _ -> Requests.post_address 4 @@ Ipaddr.V4.of_string_exn ip#get_value)
                         >>= (fun _ -> Requests.post_mask 4    @@ Ipaddr.V4.of_string_exn mask#get_value)
                         >>= (fun _ -> Requests.post_gateway 4 @@ Ipaddr.V4.of_string_exn gw#get_value)
                         >>= (fun _ -> Requests.post_reset 4)
                         >>= (fun _ -> Lwt.return_ok (apply_btn#set_disabled true)))
              apply_btn#e_click |> ignore;
  new Stateful_card.t
      ~e_state
      ~title:"Сетевые настройки"
      ~sections:[ `Media media; `Actions actions ]
      ()

let ip_settings_block e_state (cfg:Board_types.config) =
  let en_sw     = new Switch.t ~input_id:"enable" () in
  let en        = new Form_field.t ~label:"Включить приём" ~align_end:true ~input:en_sw () in
  let fec_sw    = new Switch.t ~input_id:"fec" () in
  let fec       = new Form_field.t ~label:"Включить FEC" ~align_end:true ~input:fec_sw () in
  let port      = new Textfield.t ~label:"UDP порт" ~input_type:`Number () in
  let multicast = new Textfield.t ~label:"Multicast" ~input_type:`Text () in
  let settings  = new Box.t ~widgets:[ Widget.coerce en
                                     ; Widget.coerce fec
                                     ; Widget.coerce port
                                     ; Widget.coerce multicast ] () in
  let media     = new Card.Media.t ~widgets:[settings] () in
  let apply_btn = new Button.t ~label:strings.apply () in
  let actions   = new Card.Actions.t ~widgets:[apply_btn] () in
  (* Applying styles *)
  apply_btn#set_raised false;
  apply_btn#set_compact true;
  settings#set_vertical;
  fec#style##.marginBottom := Js.string "16px";
  en#style##.marginBottom := Js.string "16px";
  (* Applying validation rules *)
  multicast#set_pattern mcast_regexp;
  port#set_min 0.0;
  port#set_max 65535.0;
  (* Inserting values *)
  en_sw#set_checked  cfg.ip.enable;
  fec_sw#set_checked cfg.ip.fec;
  port#set_value     @@ string_of_int cfg.ip.port;
  CCOpt.iter (fun x -> multicast#set_value @@ Ipaddr.V4.to_string x) cfg.ip.multicast;
  (* Listen apply button *)
  React.E.map (function
               | `No_response | `Init -> apply_btn#set_disabled true
               | _ -> apply_btn#set_disabled false) e_state |> ignore;
  React.E.map (fun () ->
      let open Lwt_result.Infix in
      apply_btn#set_disabled true;
      Requests.post_ip_enable 4 en_sw#get_checked
      >>= (fun _ -> Requests.post_fec 4 fec_sw#get_checked)
      >>= (fun _ -> Requests.post_port 4 @@ int_of_string @@ port#get_value)
      >>= (fun _ -> Requests.post_multicast 4 @@ Ipaddr.V4.of_string_exn multicast#get_value)
      >>= (fun _ -> Lwt.return_ok @@ apply_btn#set_disabled false)) apply_btn#e_click |> ignore;
  new Stateful_card.t ~e_state ~title:"Настройки приёма" ~sections:[ `Media media; `Actions actions ] ()

let load () =
  let open Lwt_result.Infix in
  Requests.get_config 4
  >>= (fun cfg ->
    let e_state     = Requests.get_state_socket 4 in
    let e_status    = Requests.get_status_socket 4 in
    let container   = Dom_html.getElementById "ip_widgets" in

    let status_card = main_status_card e_state e_status in
    let fec_card    = fec_status_card e_state e_status in
    let nw_card     = nw_settings_block e_state cfg in
    let ip_card     = ip_settings_block e_state cfg in

    let status_grid   = new Layout_grid.t ~cells:[ new Layout_grid.Cell.t ~widgets:[ status_card ] ()
                                                 ; new Layout_grid.Cell.t ~widgets:[ fec_card ] ()
                                                 ; new Layout_grid.Cell.t ~widgets:[ nw_card ] ()
                                                 ; new Layout_grid.Cell.t ~widgets:[ ip_card ] ()
                                                 ] () in
    (* List.iter (fun x -> x#set_span 3) status_grid#get_cells; *)
    (* let settings_grid = new Layout_grid.t ~cells:[ new Layout_grid.Cell.t ~widgets:[ nw_card ] ()
     *                                              ; new Layout_grid.Cell.t ~widgets:[ ip_card ] ()
     *                                              ] () in *)
    Dom.appendChild container status_grid#root;
    (* Dom.appendChild container settings_grid#root; *)
    Lwt_result.return ())
  |> ignore
