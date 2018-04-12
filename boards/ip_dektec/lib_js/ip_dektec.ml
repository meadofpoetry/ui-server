open Containers
open Components
open Lwt_result.Infix
open Ui

let format_bitrate x =
  let gbit = 1_000_000_000. in
  let mbit = 1_000_000. in
  let kbit = 1_000. in
  let (>=) = Float.(>=) in
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
  new Stateful_card.t ~title:"Статус" ~s_state ~sections:[media] ()

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
  new Stateful_card.t ~title:"Статус FEC" ~sections:[ media ] ~s_state ()

type events =
  { config : Board_types.config React.event
  ; status : Board_types.board_status React.event
  }

type listener =
  { config  : Board_types.config
  ; events  : events
  ; state   : Common.Topology.state React.signal
  ; sockets : WebSockets.webSocket Js.t list
  }

type state = (listener,string) Lwt_result.t

let listen control : state =
  Requests.get_config control
  >>= (fun cfg ->
    Requests.get_state control
    >>= (fun state ->
         let e_state,state_ws   = Requests.get_state_ws  control in
         let e_status,status_ws = Requests.get_status_ws control in
         let e_config,config_ws = Requests.get_config_ws control in
         let s_state  = React.S.hold state e_state in
         let events   = { config  = e_config; status = e_status } in
         let listener = { config  = cfg
                        ; events
                        ; state   = s_state
                        ; sockets = [ state_ws; status_ws; config_ws ]
                        }
         in
         Lwt_result.return listener))

let unlisten (x:state) =
  x >>= (fun l -> List.iter (fun x -> x##close) l.sockets; Lwt_result.return ())

class t control () = object(self)

  val mutable _state : state option = None

  inherit Widget.widget (Dom_html.createDiv Dom_html.document) ()

  method on_load =
    listen control
    >>= (fun l ->
      let s_state = React.S.map (function
                                 | `No_response | `Init -> false
                                 | `Fine -> true) l.state in
      let status_card = main_status_card s_state l.events.status in
      let fec_card    = fec_status_card s_state l.events.status in
      let status_grid   = new Layout_grid.t ~cells:[ new Layout_grid.Cell.t ~widgets:[ status_card ] ()
                                                   ; new Layout_grid.Cell.t ~widgets:[ fec_card ] ()
                                                   ] () in
      Dom.appendChild self#root status_grid#root;
      Lwt_result.return l)
    |> fun s -> _state <- Some s

  method on_unload : unit =
    match _state with
    | Some s -> unlisten s |> ignore
    | None   -> ()

end

let page control () = new t control ()
