open Containers
open Components
open Board_types.Streams.TS
open Lwt_result.Infix
open Common
open Board_types

type composition =
  { services : bool
  ; pids     : bool
  ; emm      : bool
  ; tables   : bool
  } [@@deriving yojson]

type config =
  { stream       : Stream.id
  ; auto_refresh : bool
  ; composition  : composition
  } [@@deriving yojson]

let default_config =
  { stream       = Single
  ; auto_refresh = true
  ; composition  = { services = true; pids = true; emm = true; tables = true }
  }

(* Widget default name *)
let name = "Структура"

(* Settings widget *)
let settings = None

let (^::) = List.cons_maybe

let make_pid (pid : pid_info) =
  let text, stext = Printf.sprintf "PID: %d" pid.pid,
                    let pts = if pid.has_pts then "Есть PTS" else "" in
                    let scr = if pid.scrambled then "Скремблирован" else "" in
                    String.concat ", " (List.filter (fun x -> not (String.equal x "")) [ pts; scr ]) in
  new Tree.Item.t ~text ~secondary_text:stext ()

let make_es (es : es_info) =
  let text, stext = Printf.sprintf "ES PID: %d" es.pid,
                    let typ = Printf.sprintf "Тип: %d" es.es_type in
                    let sid = Printf.sprintf "Stream ID: %d" es.es_stream_id in
                    let pts = if es.has_pts then "Есть PTS" else "" in
                    String.concat ", " (List.filter (fun x -> not (String.equal x "")) [ typ; sid; pts]) in
  new Tree.Item.t ~text ~secondary_text:stext ()

let make_ecm (ecm : ecm_info) =
  let text, stext = Printf.sprintf "ECM PID: %d" ecm.pid,
                    Printf.sprintf "CA System ID: %d" ecm.ca_sys_id in
  new Tree.Item.t ~text ~secondary_text:stext ()

let make_emm (emm : emm_info) =
  let text, stext = Printf.sprintf "EMM PID: %d" emm.pid,
                    Printf.sprintf "CA System ID: %d" emm.ca_sys_id in
  new Tree.Item.t ~text ~secondary_text:stext ()

let make_service (service : service_info) =
  let text, stext = service.name,
                    Printf.sprintf "Провайдер: %s" service.provider_name in
  let id      = new Tree.Item.t ~text:(Printf.sprintf "ID: %d" service.id) () in
  let pmt_pid = new Tree.Item.t ~text:(Printf.sprintf "PMT PID: %d" service.pmt_pid) () in
  let pcr_pid = new Tree.Item.t ~text:(Printf.sprintf "PCR PID: %d" service.pcr_pid) () in
  let es      = if not (List.is_empty service.es)
                then Some (let es = List.sort (fun (x:es_info) y -> compare x.pid y.pid) service.es in
                           new Tree.Item.t
                             ~text:"Элементарные потоки"
                             ~nested:(new Tree.t ~items:(List.map make_es es) ())
                             ())
                else None in
  let ecm     = if not (List.is_empty service.ecm)
                then Some (let ecm = List.sort (fun (x:ecm_info) y -> compare x.pid y.pid) service.ecm in
                           new Tree.Item.t
                             ~text:"ECM"
                             ~nested:(new Tree.t ~items:(List.map make_ecm ecm) ())
                             ())
                else None in
  let opt     = es ^:: ecm ^:: [] in
  let nested  = new Tree.t ~items:([ id; pmt_pid; pcr_pid ] @ opt) () in
  let graphic = new Icon.Font.t ~icon:"tv" () in
  new Tree.Item.t ~text ~secondary_text:stext ~graphic ~nested ()

let make_section (s : section_info) =
  let text, stext = Printf.sprintf "ID: %d" s.id,
                    Printf.sprintf "Длина: %d" s.length in
  new Tree.Item.t ~text ~secondary_text:stext ()

let make_table (table : table) =
  let common = table_common_of_table table in
  let text,stext = Printf.sprintf "%s, PID: %d" (table_to_string table) common.pid,
                   Printf.sprintf "Версия: %d, ID: %d, LSN: %d" common.version common.id common.lsn in
  let specific = match table with
    | PAT x -> [ new Tree.Item.t ~text:(Printf.sprintf "TS ID: %d" x.ts_id) () ]
    | PMT x -> [ new Tree.Item.t ~text:(Printf.sprintf "Номер программы: %d" x.program_number) () ]
    | NIT x -> [ new Tree.Item.t ~text:(Printf.sprintf "Network ID: %d" x.nw_id) () ]
    | SDT x -> [ new Tree.Item.t ~text:(Printf.sprintf "TS ID: %d" x.ts_id) () ]
    | BAT x -> [ new Tree.Item.t ~text:(Printf.sprintf "Bouquet ID: %d" x.bouquet_id) () ]
    | EIT x -> [ new Tree.Item.t ~text:(Printf.sprintf "Service ID: %d" x.service_id) ()
               ; new Tree.Item.t ~text:(Printf.sprintf "TS ID: %d" x.params.ts_id) ()
               ; new Tree.Item.t ~text:(Printf.sprintf "Oririnal network ID: %d" x.params.orig_nw_id) ()
               ; new Tree.Item.t ~text:(Printf.sprintf "Segment LSN: %d" x.params.segment_lsn) ()
               ; new Tree.Item.t ~text:(Printf.sprintf "Last table ID: %d" x.params.last_table_id) () ]
    | _     -> []
  in
  let sections = new Tree.Item.t
                   ~text:(Printf.sprintf "Секции (%d)" @@ List.length common.sections)
                   ~nested:(new Tree.t ~items:(List.map make_section common.sections) ())
                   () in
  let nested = new Tree.t ~items:(specific @ [sections]) () in
  new Tree.Item.t ~text ~secondary_text:stext ~nested ()

let make_general (ts : general_info) =
  let items = [ new Tree.Item.t ~text:(Printf.sprintf "Network PID: %d" ts.nw_pid) ()
              ; new Tree.Item.t ~text:(Printf.sprintf "TS ID: %d" ts.ts_id) ()
              ; new Tree.Item.t ~text:(Printf.sprintf "Network ID: %d" ts.nw_id) ()
              ; new Tree.Item.t ~text:(Printf.sprintf "Original Network ID: %d" ts.orig_nw_id) ()
              ; new Tree.Item.t ~text:(Printf.sprintf "Network name: %s" ts.nw_name) ()
              ]
  in
  let nested = new Tree.t ~items () in
  new Tree.Item.t ~text:"Сведения о потоке" ~nested ()

let make_stream (ts : structure) =
  let gen  = make_general ts.general in
  let pids =
    if not (List.is_empty ts.pids)
    then Some (let pids = List.sort (fun (x:pid_info) y -> compare x.pid y.pid) ts.pids in
               new Tree.Item.t
                 ~text:"PIDs"
                 ~nested:(new Tree.t ~items:(List.map make_pid pids) ())
                 ())
    else None in
  let serv =
    if not (List.is_empty ts.services)
    then Some (let serv = List.sort (fun (x:service_info) y -> compare x.id y.id) ts.services in
               new Tree.Item.t
                 ~text:"Сервисы"
                 ~nested:(new Tree.t ~items:(List.map make_service serv) ())
                 ())
    else None in
  let emm  =
    if not (List.is_empty ts.emm)
    then Some (let emm = List.sort (fun (x:emm_info) y -> compare x.pid y.pid) ts.emm in
               new Tree.Item.t
                 ~text:"EMM"
                 ~nested:(new Tree.t ~items:(List.map make_emm emm) ())
                 ())
    else None in
  let tabl =
    if not (List.is_empty ts.tables)
    then Some (let tabl = List.sort (fun (x:table) y -> compare (table_common_of_table x).pid
                                                          (table_common_of_table y).pid)
                            ts.tables in
               new Tree.Item.t
                 ~text:"Таблицы"
                 ~nested:(new Tree.t ~items:(List.map make_table tabl) ())
                 ())
    else None in
  let opt    = serv ^:: tabl ^:: pids ^:: emm ^:: [] in
  new Tree.t ~items:(gen :: opt) ()

let make
      ?(config=default_config)
      ~(state:Common.Topology.state React.signal)
      ~(signal:(Stream.id * structure) list React.signal)
      () =
  (* FIXME remove *)
  let signal,push = React.S.create [] in
  let open Lwt.Infix in
  let _ = Api_js.Requests.Json_request.get
            ?scheme:None ?host:None ?port:None
            ~path:Uri.Path.Format.("/js/structure.json" @/ empty)
            ~query:Uri.Query.empty
          >|= (fun r ->
      Result.get_exn r
      |> Json.(List.of_yojson (Pair.of_yojson Stream.id_of_yojson Streams.TS.structure_of_yojson))
      |> Result.get_exn
      |> push)
  in
  (* FIXME end of temp block *)
  let div = Dom_html.createDiv Dom_html.document |> Widget.create in
  let ph  = Ui_templates.Placeholder.create_with_icon
              ~icon:"warning" ~text:"Поток отсутствует" () in
  let ()  =
    React.S.map (function
        | None   -> div#set_empty ();
                    Dom.appendChild div#root ph#root
        | Some s ->
           div#set_empty ();
           let stream  = make_stream s in
           let hexdump = new Hexdump.t ~config:(Hexdump.to_config ~width:8 ())
                           "abcdefg" () in
           let split   = new Hsplit.t stream hexdump () in
           Dom.appendChild div#root split#root)
    @@ React.S.map (List.Assoc.get ~eq:Stream.equal_id config.stream) signal
    |> Lwt_react.S.keep
  in
  div
