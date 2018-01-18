open Components
open Board_types

let (^::) = CCList.cons_maybe

let make_pid (pid : pid) =
  let text, stext = Printf.sprintf "PID: %d" pid.pid,
                    let pts = if pid.has_pts then "Есть PTS" else "" in
                    let scr = if pid.scrambled then "Скремблирован" else "" in
                    CCString.concat ", " (CCList.filter (fun x -> x <> "") [ pts; scr ]) in
  new Tree.Item.t ~text ~secondary_text:stext ()

let make_es (es : es) =
  let text, stext = Printf.sprintf "ES PID: %d" es.pid,
                    let typ = Printf.sprintf "Тип: %d" es.es_type in
                    let sid = Printf.sprintf "Stream ID: %d" es.es_stream_id in
                    let pts = if es.has_pts then "Есть PTS" else "" in
                    CCString.concat ", " (CCList.filter (fun x -> x <> "") [ typ; sid; pts]) in
  new Tree.Item.t ~text ~secondary_text:stext ()

let make_ecm (ecm : ecm ) =
  let text, stext = Printf.sprintf "ECM PID: %d" ecm.pid,
                    Printf.sprintf "CA System ID: %d" ecm.ca_sys_id in
  new Tree.Item.t ~text ~secondary_text:stext ()

let make_emm (emm : emm ) =
  let text, stext = Printf.sprintf "EMM PID: %d" emm.pid,
                    Printf.sprintf "CA System ID: %d" emm.ca_sys_id in
  new Tree.Item.t ~text ~secondary_text:stext ()

let make_service (service : service) =
  let text, stext = Printf.sprintf "Имя: %s" service.name,
                    Printf.sprintf "Провайдер: %s" service.provider_name in
  let id      = new Tree.Item.t ~text:(Printf.sprintf "ID: %d" service.id) () in
  let pmt_pid = new Tree.Item.t ~text:(Printf.sprintf "PMT PID: %d" service.pmt_pid) () in
  let pcr_pid = new Tree.Item.t ~text:(Printf.sprintf "PCR PID: %d" service.pcr_pid) () in
  let es      = if not (CCList.is_empty service.es)
                then Some (let es = CCList.sort (fun (x:es) y -> compare x.pid y.pid) service.es in
                           new Tree.Item.t
                               ~text:"Элементарные потоки"
                               ~nested:(new Tree.t ~items:(CCList.map make_es es) ())
                               ())
                else None in
  let ecm     = if not (CCList.is_empty service.ecm)
                then Some (let ecm = CCList.sort (fun (x:ecm) y -> compare x.pid y.pid) service.ecm in
                           new Tree.Item.t
                               ~text:"ECM"
                               ~nested:(new Tree.t ~items:(CCList.map make_ecm ecm) ())
                               ())
                else None in
  let opt     = es ^:: ecm ^:: [] in
  let nested  = new Tree.t ~items:([ id; pmt_pid; pcr_pid ] @ opt) () in
  new Tree.Item.t ~text ~secondary_text:stext ~nested ()

let make_section (s : table_section) =
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
               ; new Tree.Item.t ~text:(Printf.sprintf "TS ID: %d" x.eit_info.ts_id) ()
               ; new Tree.Item.t ~text:(Printf.sprintf "Oririnal network ID: %d" x.eit_info.orig_nw_id) ()
               ; new Tree.Item.t ~text:(Printf.sprintf "Segment LSN: %d" x.eit_info.segment_lsn) ()
               ; new Tree.Item.t ~text:(Printf.sprintf "Last table ID: %d" x.eit_info.last_table_id) () ]
    | _     -> []
  in
  let sections = new Tree.Item.t
                     ~text:(Printf.sprintf "Секции (%d)" @@ CCList.length common.sections)
                     ~nested:(new Tree.t ~items:(CCList.map make_section common.sections) ())
                     () in
  let nested = new Tree.t ~items:(specific @ [sections]) () in
  new Tree.Item.t ~text ~secondary_text:stext ~nested ()

let make_stream (ts : ts_struct) =
  let pids = if not (CCList.is_empty ts.pids)
             then Some (let pids = CCList.sort (fun (x:pid) y -> compare x.pid y.pid) ts.pids in
                        new Tree.Item.t
                            ~text:"PIDs"
                            ~nested:(new Tree.t ~items:(CCList.map make_pid pids) ())
                            ())
             else None in
  let serv = if not (CCList.is_empty ts.services)
             then Some (let serv = CCList.sort (fun (x:service) y -> compare x.id y.id) ts.services in
                        new Tree.Item.t
                            ~text:"Сервисы"
                            ~nested:(new Tree.t ~items:(CCList.map make_service serv) ())
                            ())
             else None in
  let emm  = if not (CCList.is_empty ts.emm)
             then Some (let emm = CCList.sort (fun (x:emm) y -> compare x.pid y.pid) ts.emm in
                        new Tree.Item.t
                            ~text:"EMM"
                            ~nested:(new Tree.t ~items:(CCList.map make_emm emm) ())
                            ())
             else None in
  let tabl = if not (CCList.is_empty ts.tables)
             then Some (let tabl = CCList.sort (fun (x:table) y -> compare (table_common_of_table x).pid
                                                                           (table_common_of_table y).pid)
                                               ts.tables in
                        new Tree.Item.t
                            ~text:"Таблицы"
                            ~nested:(new Tree.t ~items:(CCList.map make_table tabl) ())
                            ())
             else None in
  let text, stext = "Поток", "" in
  let opt    = serv ^:: tabl ^:: pids ^:: emm ^:: [] in
  let nested = new Tree.t ~items:([ ] @ opt) () in
  new Tree.Item.t ~text ~secondary_text:stext ~nested ()

let make_streams_tree (ts : ts_struct list) =
  let ts   = CCList.map make_stream ts in
  let tree = new Tree.t ~items:ts () in
  tree#set_dense true;
  tree#style##.maxWidth := Js.string "700px";
  tree

let create
      ~(div   : Dom_html.element Js.t)
      ~(init  : ts_structs)
      ~(event : ts_structs React.event) =
  let id = "ts-structures" in
  let make (ts : ts_structs) =
    let tree = make_streams_tree ts in
    tree#set_id id;
    tree
  in
  let _ = React.E.map (fun s ->
              (try Dom.removeChild div (Dom_html.getElementById id)
               with _ -> ());
              Dom.appendChild div (make s)#root)
                      event
  in
  Dom.appendChild div (make init)#root

let page control =
  let open Lwt_result.Infix in
  let div = Dom_html.createDiv Dom_html.document in
  let t =
    Requests.get_structs control
    >>= (fun init ->
      let e_structs,sock = Requests.get_structs_ws control in
      create ~div ~init ~event:e_structs;
      Lwt_result.return sock)
  in
  div,(fun () -> t >>= (fun x -> Lwt_result.return x##close) |> ignore)
