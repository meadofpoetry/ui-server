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
  (* ; auto_refresh : bool
   * ; composition  : composition *)
  } [@@deriving yojson]

let default_config =
  { stream       = Single
  (* ; auto_refresh = true
   * ; composition  = { services = true; pids = true; emm = true; tables = true } *)
  }

type dumpable =
  { name   : string * string
  ; get    : unit -> (string,string) Lwt_result.t
  ; prev   : string option React.signal
  }

(* Widget default name *)
let name = "Структура"

(* Settings widget *)
let settings = None

let (^::) = List.cons_maybe

let base_class = "qos-niit-structure"

let make_pid (pid : pid_info) =
  let text, stext = Printf.sprintf "PID: %d" pid.pid, None in
  let scrambled = match pid.scrambled with
    | true  -> Some (new Icon.SVG.t ~icon:Lock ())
    | false -> None in
  let pts = match pid.has_pts with
    | true  -> Some (new Icon.SVG.t ~icon:Clock_outline ())
    | false -> None in
  let meta = match scrambled,pts with
    | Some s, Some pts -> Some (new Hbox.t ~widgets:[s;pts] ())#widget
    | Some s, None     -> Some s#widget
    | None, Some pts   -> Some pts#widget
    | None, None       -> None
  in
  new Tree.Item.t ~text ?meta ?secondary_text:stext ~value:() ()

let make_es (es : es_info) =
  let text, stext = Printf.sprintf "ES PID: %d" es.pid,
                    let typ = Printf.sprintf "Тип: %d" es.es_type in
                    let sid = Printf.sprintf "Stream ID: %d" es.es_stream_id in
                    let pts = if es.has_pts then "Есть PTS" else "" in
                    String.concat ", " (List.filter (fun x -> not (String.equal x "")) [ typ; sid; pts]) in
  new Tree.Item.t ~text ~secondary_text:stext ~value:() ()

let make_ecm (ecm : ecm_info) =
  let text, stext = Printf.sprintf "ECM PID: %d" ecm.pid,
                    Printf.sprintf "CA System ID: %d" ecm.ca_sys_id in
  new Tree.Item.t ~text ~secondary_text:stext ~value:() ()

let make_emm (emm : emm_info) =
  let text, stext = Printf.sprintf "EMM PID: %d" emm.pid,
                    Printf.sprintf "CA System ID: %d" emm.ca_sys_id in
  new Tree.Item.t ~text ~secondary_text:stext ~value:() ()

let make_service (service : service_info) =
  let text, stext = service.name,
                    Printf.sprintf "Провайдер: %s" service.provider_name in
  let id =
    new Tree.Item.t
      ~text:(Printf.sprintf "ID: %d" service.id)
      ~value:()
      () in
  let pmt_pid =
    new Tree.Item.t
      ~text:(Printf.sprintf "PMT PID: %d" service.pmt_pid)
      ~value:()
      () in
  let pcr_pid =
    new Tree.Item.t
      ~text:(Printf.sprintf "PCR PID: %d" service.pcr_pid)
      ~value:()
      () in
  let es =
    if not (List.is_empty service.es)
    then Some (let es = List.sort (fun (x:es_info) y -> compare x.pid y.pid) service.es in
               new Tree.Item.t
                 ~text:"Элементарные потоки"
                 ~nested:(new Tree.t ~items:(List.map make_es es) ())
                 ~value:()
                 ())
    else None in
  let ecm =
    if not (List.is_empty service.ecm)
    then Some (let ecm = List.sort (fun (x:ecm_info) y -> compare x.pid y.pid) service.ecm in
               new Tree.Item.t
                 ~text:"ECM"
                 ~nested:(new Tree.t ~items:(List.map make_ecm ecm) ())
                 ~value:()
                 ())
    else None in
  let opt     = es ^:: ecm ^:: [] in
  let nested  = new Tree.t ~items:([ id; pmt_pid; pcr_pid ] @ opt) () in
  let graphic = new Icon.SVG.t ~icon:Tv () in
  new Tree.Item.t ~text ~secondary_text:stext ~graphic ~nested ~value:() ()

let req_of_table stream section tbl control =
  let common = table_common_of_table tbl in
  let r = Requests.Stream.HTTP.TS.get_si_psi_section
            ~id:stream
            ~table_id:common.id
            ~section in
  match tbl with
  | PAT x -> r ~table_id_ext:x.ts_id control
  | PMT x -> r ~table_id_ext:x.program_number control
  | NIT x -> r ~table_id_ext:x.nw_id control
  | SDT x -> r ~table_id_ext:x.ts_id control
  | BAT x -> r ~table_id_ext:x.bouquet_id control
  | EIT x -> r ~table_id_ext:x.service_id
               ~eit_ts_id:x.params.ts_id
               ~eit_orig_nw_id:x.params.orig_nw_id
               control
  | _     -> r control

let make_section_name table section =
  let divider = ", " in
  let common  = table_common_of_table table in
  let name    = table_to_string table in
  let id s x  = Printf.sprintf "%s=0x%02X(%d)" s x x in
  let base    = id "table_id" common.id in
  let section = Printf.sprintf "секция %d" section in
  let specific = match table with
    | PAT x -> Some [ id "ts_id" x.ts_id ]
    | PMT x -> Some [ id "program_number" x.program_number ]
    | NIT x -> Some [ id "network_id" x.nw_id ]
    | SDT x -> Some [ id "ts_id" x.ts_id ]
    | BAT x -> Some [ id "bouquet_id" x.bouquet_id ]
    | EIT x -> Some [ id "service_id" x.service_id
                    ; id "ts_id" x.params.ts_id
                    ; id "original_network_id" x.params.orig_nw_id ]
    | _     -> None in
  match specific with
  | Some l -> name, String.concat divider (base :: l @ [ section ])
  | None   -> name, base ^ divider ^ section

let make_section stream table ({id;length;_}:section_info) control =
  let open Lwt_result.Infix in
  let base_class  = Markup.CSS.add_element base_class "section-item" in
  let req ()      = req_of_table stream id table control in
  let prev,push   = React.S.create None in
  let text, stext = Printf.sprintf "ID: %d" id, None in
  let graphic = new Icon.SVG.t ~icon:Download () in
  let meta    = Dom_html.createSpan Dom_html.document |> Widget.create in
  let byte_s  = if length > 1 && length < 5 then "байта" else "байт" in
  let str  = Printf.sprintf "%d %s" length byte_s in
  let ()   = meta#set_text_content str in
  let get  = fun () ->
    req ()
    >|= (fun dump -> push (Some dump.section); dump.section)
    |> Lwt_result.map_err Api_js.Requests.err_to_string in
  let item = new Tree.Item.t ~text ?secondary_text:stext
               ~graphic ~meta ~value:() () in
  let e,e_push = React.E.create () in
  let name     = make_section_name table id in
  Dom_events.listen item#item#root Dom_events.Typ.click (fun _ _ ->
      let dumpable = { name; get; prev } in
      e_push dumpable; false) |> ignore;
  let () = item#add_class base_class in
  item,e

let make_table (stream:Stream.id) (table:table) control =
  let common = table_common_of_table table in
  let text,stext = Printf.sprintf "%s, PID: %d" (table_to_string table) common.pid,
                   Printf.sprintf "Версия: %d, ID: %d, LSN: %d" common.version common.id common.lsn in
  let make_item = new Tree.Item.t ~value:() in
  let specific = match table with
    | PAT x -> [ make_item ~text:(Printf.sprintf "TS ID: %d" x.ts_id) () ]
    | PMT x -> [ make_item ~text:(Printf.sprintf "Номер программы: %d" x.program_number) () ]
    | NIT x -> [ make_item ~text:(Printf.sprintf "Network ID: %d" x.nw_id) () ]
    | SDT x -> [ make_item ~text:(Printf.sprintf "TS ID: %d" x.ts_id) () ]
    | BAT x -> [ make_item ~text:(Printf.sprintf "Bouquet ID: %d" x.bouquet_id) () ]
    | EIT x -> [ make_item ~text:(Printf.sprintf "Service ID: %d" x.service_id) ()
               ; make_item ~text:(Printf.sprintf "TS ID: %d" x.params.ts_id) ()
               ; make_item ~text:(Printf.sprintf "Oririnal network ID: %d" x.params.orig_nw_id) ()
               ; make_item ~text:(Printf.sprintf "Segment LSN: %d" x.params.segment_lsn) ()
               ; make_item ~text:(Printf.sprintf "Last table ID: %d" x.params.last_table_id) () ]
    | _     -> []
  in
  let items,e  = List.map (fun x ->
                     make_section stream table x control) common.sections
                 |> List.split in
  let e        = React.E.select e in
  let sections =
    new Tree.Item.t
      ~text:(Printf.sprintf "Секции (%d)" @@ List.length common.sections)
      ~nested:(new Tree.t ~items ())
      ~value:()
      () in
  let nested = new Tree.t ~items:(specific @ [sections]) () in
  new Tree.Item.t ~text ~secondary_text:stext ~nested ~value:() (),e

let make_general (ts : general_info) =
  let make_item = new Tree.Item.t ~ripple:false ~value:() in
  let items =
    [ make_item ~text:(Printf.sprintf "Network PID: %d" ts.nw_pid) ()
    ; make_item ~text:(Printf.sprintf "TS ID: %d" ts.ts_id) ()
    ; make_item ~text:(Printf.sprintf "Network ID: %d" ts.nw_id) ()
    ; make_item ~text:(Printf.sprintf "Original Network ID: %d"
                         ts.orig_nw_id) ()
    ; make_item ~text:(Printf.sprintf "Network name: %s" ts.nw_name) ()
    ] in
  let nested = new Tree.t ~items () in
  new Tree.Item.t ~text:"Сведения о потоке" ~nested ~value:() ()

let make_stream (id:Stream.id) (ts : structure) control =
  let gen  = make_general ts.general in
  let pids =
    if not (List.is_empty ts.pids)
    then Some (let pids = List.sort (fun (x:pid_info) y ->
                              compare x.pid y.pid) ts.pids in
               new Tree.Item.t
                 ~text:"PIDs"
                 ~nested:(new Tree.t ~items:(List.map make_pid pids) ())
                 ~value:()
                 ())
    else None in
  let serv =
    if not (List.is_empty ts.services)
    then Some (let serv = List.sort (fun (x:service_info) y ->
                              compare x.id y.id) ts.services in
               new Tree.Item.t
                 ~text:"Сервисы"
                 ~nested:(new Tree.t ~items:(List.map make_service serv) ())
                 ~value:()
                 ())
    else None in
  let emm  =
    if not (List.is_empty ts.emm)
    then Some (let emm = List.sort (fun (x:emm_info) y ->
                             compare x.pid y.pid) ts.emm in
               new Tree.Item.t
                 ~text:"EMM"
                 ~nested:(new Tree.t ~items:(List.map make_emm emm) ())
                 ~value:()
                 ())
    else None in
  let tabl,e =
    if not (List.is_empty ts.tables)
    then
      let tabl =
        List.sort (fun (x:table) y -> compare (table_common_of_table x).pid
                                        (table_common_of_table y).pid)
          ts.tables in
      let items,e = List.map (fun x -> make_table id x control) tabl
                    |> List.split in
      let e       = React.E.select e in
      let nested  = new Tree.t ~items () in
      new Tree.Item.t ~text:"Таблицы" ~nested ~value:() ()
      |> fun x -> Option.return x, e
    else None, React.E.never in
  let opt    = serv ^:: tabl ^:: pids ^:: emm ^:: [] in
  let tree = new Tree.t ~items:(gen :: opt) () in
  let ()   = tree#set_dense true in
  tree, e

let make_parsed (event:dumpable React.event) =
  (* CSS classes *)
  let base_class   = Markup.CSS.add_element base_class "parsed" in
  let header_class = Markup.CSS.add_element base_class "header" in
  let title_class  = Markup.CSS.add_element base_class "title" in
  let body_class   = Markup.CSS.add_element base_class "body" in
  (* Signal for hexdump element *)
  let s, p = React.S.create "" in
  (* Elements *)
  let title     = new Typography.Text.t
                    ~adjust_margin:false
                    ~text:"Выберите секцию таблицы SI/PSI для захвата" () in
  let subtitle  = new Typography.Text.t
                    ~adjust_margin:false
                    ~split:true
                    ~text:"" () in
  let button    = new Ui_templates.Buttons.Get.t
                    ~style:`Raised
                    ~label:"Загрузить" () in
  let body      = Dom_html.createDiv Dom_html.document |> Widget.create in
  let title_box = new Vbox.t
                    ~widgets:[ title#widget
                             ; subtitle#widget ] () in
  let header    = new Hbox.t
                    ~halign:`Space_between
                    ~widgets:[ title_box#widget
                             ; button#widget] () in
  let box       = new Vbox.t
                    ~widgets:[ header#widget
                             ; (new Divider.t ())#widget
                             ; body#widget ] () in
  (* CSS classes setup *)
  let ()   = title#add_class title_class in
  let ()   = header#add_class header_class in
  let ()   = body#add_class body_class in
  let ()   = box#add_class base_class in
  let _e   =
    React.E.map (fun {name;get;prev} ->
        let open Lwt.Infix in
        let text   = new Typography.Text.t ~text:"" () in
        let err x  = Ui_templates.Placeholder.create_with_error ~text:x () in
        let ph  x  = Ui_templates.Placeholder.create_with_icon
                       ~icon:"info"
                       ~text:x () in
        let get    = fun () ->
          Lwt.catch (fun () ->
              get ()
              >|= (function
                   | Ok _    -> body#set_empty ();
                                Dom.appendChild body#root text#root
                   | Error s -> body#set_empty ();
                                Dom.appendChild body#root (err s)#root))
            (fun e ->
              body#set_empty ();
              Dom.appendChild body#root (err @@ Printexc.to_string e)#root;
              Lwt.return_unit) in
        let _s   =
          React.S.map (function
              | Some raw ->
                 body#set_empty ();
                 text#set_text raw;
                 Dom.appendChild body#root text#root;
                 p raw
              | None     ->
                 body#set_empty ();
                 Dom.appendChild body#root (ph "Нет захваченных данных")#root;
                 p "") prev in
        let () = button#set_getter (Some get) in
        let () = title#set_text @@ fst name in
        let () = subtitle#set_text @@ snd name in
        let () = button#set_disabled false in
        _s) event
    |> Lwt_react.E.keep
  in
  box#widget,s

let make_hexdump (signal:string React.signal) =
  let base_class = Markup.CSS.add_element base_class "hexdump" in
  let config     = Hexdump.to_config ~width:16 () in
  let base =
    new Select.t
      ~label:"Основание"
      ~items:[ `Item (new Select.Item.t ~value:`Hex ~text:"16" ())
             ; `Item (new Select.Item.t ~value:`Dec ~text:"10" ())
             ; `Item (new Select.Item.t ~value:`Bin ~text:"2" ()) ]
      () in
  let width =
    new Select.t
      ~label:"Ширина"
      ~items:[ `Item (new Select.Item.t ~value:4  ~text:"4"  ())
             ; `Item (new Select.Item.t ~value:8  ~text:"8"  ~selected:true ())
             ; `Item (new Select.Item.t ~value:16 ~text:"16" ())
             ; `Item (new Select.Item.t ~value:32 ~text:"32" ()) ]
      () in
  let line_numbers  = new Switch.t ~state:true () in
  let line_numbers' = new Form_field.t ~input:line_numbers
                        ~label:"Номера" () in
  let options = new Hbox.t
                  ~widgets:[ base#widget
                           ; width#widget
                           ; line_numbers'#widget ] () in
  let hexdump = new Hexdump.t ~config "" () in
  let _s = React.S.map hexdump#set_bytes signal in
  Lwt_react.S.keep _s;
  let () = options#add_class @@ Markup.CSS.add_element base_class "options" in
  let _  = React.S.map hexdump#set_line_numbers line_numbers#s_state in
  let _  = React.S.map (function
               | Some x -> hexdump#set_width x
               | None   -> ()) width#s_selected_value in
  let _  = React.S.map (function
               | Some x -> hexdump#set_base x
               | None   -> ()) base#s_selected_value in
  let box = new Vbox.t
              ~valign:`End
              ~widgets:[ hexdump#widget
                       ; (new Divider.t ())#widget
                       ; options#widget ]
              () in
  let () = box#add_class base_class in
  box#widget

let make_dump (event:dumpable React.event) =
  let base_class = Markup.CSS.add_element base_class "dump" in
  let parsed, s  = make_parsed event in
  let hexdump    = make_hexdump s in
  let vbox       =
    new Vbox.t
      ~widgets:[ parsed#widget
               ; (new Divider.t ())#widget
               ; hexdump#widget ]
      () in
  vbox#add_class base_class;
  vbox#widget

let make
      ?(config=default_config)
      ~(state:Common.Topology.state React.signal)
      ~(signal:(Stream.id * structure) list React.signal)
      (control:int)
      () =
  let stream_panel_class = Markup.CSS.add_element base_class "stream-panel" in
  let stream = config.stream in
  let ph  = Ui_templates.Placeholder.create_with_icon
              ~icon:"warning" ~text:"Нет потока" () in
  let stream_box = Dom_html.createDiv Dom_html.document
                   |> Widget.create in
  let e =
    React.S.map ~eq:(fun _ _ -> false) (function
        | None   ->
           stream_box#set_empty ();
           Dom.appendChild stream_box#root ph#root;
           React.E.never
        | Some s ->
           stream_box#set_empty ();
           let stream, e = make_stream stream s control in
           Dom.appendChild stream_box#root stream#root;
           e)
    @@ React.S.map (List.Assoc.get ~eq:Stream.equal_id stream) signal
    |> React.S.changes
    |> React.E.switch React.E.never in
  let dump   = make_dump e in
  let hsplit = new Hsplit.t stream_box dump () in
  let () = stream_box#add_class stream_panel_class in
  let () = hsplit#add_class base_class in
  hsplit#widget
