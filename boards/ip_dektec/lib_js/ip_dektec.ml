open Components

let format_bitrate x =
  let gbit = 1_000_000_000. in
  let mbit = 1_000_000. in
  let kbit = 1_000. in
  let v,s = (match (float_of_int x) with
             | x when x >= gbit -> x /. gbit, "Gbit/s"
             | x when x >= mbit -> x /. mbit, "Mbit/s"
             | x when x >= kbit -> x /. kbit, "Kbit/s"
             | x -> x, "Bit/s") in
  Printf.sprintf "%.2f %s" v s

let status_row name e f =
  let open Tyxml_js.Html in
  let nw = div [pcdata name] |> Tyxml_js.To_dom.of_element |> Widget.create in
  let vw = div [pcdata ""] |> Tyxml_js.To_dom.of_element |> Widget.create in
  Typography.set ~font:Subheading_1 nw;
  Typography.set ~font:Body_1 vw;
  React.E.map (fun s -> vw#set_text_content @@ f s) e |> ignore;
  div ~a:[ a_style "display:flex; justify-content:space-between;margin-bottom:10px"]
      (Widget.widgets_to_markup [nw; vw])

let fec_status_card (e:Board_types.board_status React.event) =
  let title    = new Card.Title.t ~title:"FEC status" ~large:true () in
  let primary  = new Card.Primary.t ~widgets:[ title ] () in
  let open Tyxml_js.Html in
  let params   = div [ status_row "Fec delay"            e (fun s -> string_of_int s.fec_delay)
                     ; status_row "Fec columns"          e (fun s -> string_of_int s.fec_cols)
                     ; status_row "Fec rows"             e (fun s -> string_of_int s.fec_rows)
                     ; status_row "Lost after fec"       e (fun s -> Int64.to_string s.lost_after_fec)
                     ; status_row "Lost before fec"      e (fun s -> Int64.to_string s.lost_before_fec)
                     ]
                 |> Tyxml_js.To_dom.of_element
                 |> Widget.create in
  let media    = new Card.Media.t ~widgets:[params] () in
  new Card.t ~sections:[ `Primary primary; `Media media ] ()

let main_status_card (e:Board_types.board_status React.event) =
  let title    = new Card.Title.t ~title:"Status" ~large:true () in
  let primary  = new Card.Primary.t ~widgets:[title] () in
  let open Tyxml_js.Html in
  let params   = div [ status_row "TP per IP"            e (fun s -> string_of_int s.tp_per_ip)
                     ; status_row "Protocol"             e (fun s -> Board_types.protocol_to_string s.protocol)
                     ; status_row "Packet size"          e (fun s -> Board_types.packet_sz_to_string s.packet_size)
                     ; status_row "Input bitrate"        e (fun s -> format_bitrate s.bitrate)
                     ; status_row "Output bitrate"       e (fun s -> format_bitrate s.asi_bitrate)
                     ; status_row "PCR present"          e (fun s -> string_of_bool s.pcr_present)
                     (* ; status_row "Jitter tolerance"     e (fun s -> string_of_int s.jitter_tol)
                      * ; status_row "Rate change counter"  e (fun s -> Int32.to_string s.rate_change_cnt)
                      * ; status_row "Jitter error counter" e (fun s -> Int32.to_string s.jitter_err_cnt)
                      * ; status_row "Lock error counter"   e (fun s -> Int32.to_string s.lock_err_cnt)
                      * ; status_row "Delay factor"         e (fun s -> Int32.to_string s.delay_factor) *)
                     ]
                 |> Tyxml_js.To_dom.of_element
                 |> Widget.create in
  let media    = new Card.Media.t ~widgets:[params] () in

  new Card.t ~sections:[ `Primary primary; `Media media ] ()

let load () =
  let container = Dom_html.getElementById "ip_widgets" in
  let status_ev   = Requests.get_status_socket 4 in
  let status_card = main_status_card status_ev in
  let fec_card    = fec_status_card status_ev in
  let grid = new Layout_grid.t ~cells:[ new Layout_grid.Cell.t ~widgets:[ status_card ] ()
                                      ; new Layout_grid.Cell.t ~widgets:[ fec_card ] () ] () in
  Dom.appendChild container grid#root
