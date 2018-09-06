open Containers
open Components
open Api_js.Requests
open Lwt_result.Infix
open Common
open Board_types.Streams.TS

let ( >>* ) x f = Lwt_result.map_err f x
let ( % ) = Fun.( % )

let get_errors ?from ?till ?duration ?limit ~id control =
  let open Requests.Streams.HTTP.Errors in
  get_errors ?limit ?from ?till ?duration ~id control
  >>* Api_js.Requests.err_to_string
  >>= function
  | Raw s -> Lwt_result.return s.data
  | _ -> Lwt.fail_with "got compressed"

let dummy_tab = fun () ->
  Ui_templates.Placeholder.under_development ()

let errors ({ id; _ } : Stream.t) control =
  let e, sock = Requests.Streams.WS.Errors.get_errors ~id control in
  let overview =
    get_errors ~limit:20 ~id control
    >|= (fun x -> Widget_errors_log.make ~id x control)
    >|= (fun w ->
      (* FIXME save event *)
      (* let _ = React.E.map (List.map w#add_error) e in *)
      w)
    >|= Widget.coerce
    |> Ui_templates.Loader.create_widget_loader in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let overview_cell = new Cell.t ~span ~widgets:[ overview ] () in
    let cells =
      [ new Cell.t ~span ~widgets:[ new Text.t ~text:"Обзор" ()] ()
      ; overview_cell ] in
    new t ~cells () in
  box#set_on_destroy @@ Some (fun () -> sock##close);
  box#widget

let services ({ id; _ } as stream : Stream.t) control =
  let overview = Widget_services_overview.make stream control in
  let sock_lwt =
    overview#thread
    >|= fun w ->
    let rate, rate_sock = Requests.Streams.WS.get_bitrate ~id control in
    let pids, pids_sock = Requests.Streams.WS.get_pids ~id control in
    let e, sock = Requests.Streams.WS.get_services ~id  control in
    let e = React.E.map w#update e in
    let pids = React.E.map w#update_pids pids in
    let rate = React.E.map (w#set_rate % Option.return) rate in
    e, sock, pids, pids_sock, rate, rate_sock in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let overview_cell = new Cell.t ~span ~widgets:[ overview ] () in
    let cells = [overview_cell] in
    new t ~cells () in
  box#set_on_destroy
  @@ Some (fun () ->
         sock_lwt
         >|= (fun (e, sock, pids, pids_sock, rate, rate_sock) ->
             React.E.stop ~strong:true e;
             React.E.stop ~strong:true rate;
             React.E.stop ~strong:true pids;
             sock##close;
             rate_sock##close;
             pids_sock##close)
         |> Lwt.ignore_result);
  box#widget

let pids ({ id; _ } as stream : Stream.t) control =
  let init = Requests.Streams.HTTP.get_last_pids ~id control in
  let summary = Widget_pids_summary.make ~init stream control in
  let overview = Widget_pids_overview.make ~init stream control in
  let ws_lwt =
    Lwt.choose [ summary#thread >|= Widget.coerce
               ; overview#thread >|= Widget.coerce ]
    >|= fun _ ->
    (Requests.Streams.WS.get_bitrate ~id control),
    (Requests.Streams.WS.get_pids ~id control) in
  let rate =
    Lwt_react.E.delay
    @@ Lwt.bind ws_lwt
         (function
          | Ok (x, _) -> Lwt.return @@ fst x
          | Error e -> Lwt.fail_with e) in
  let pids =
    Lwt_react.E.delay
    @@ Lwt.bind ws_lwt
         (function
          | Ok (_, x) -> Lwt.return @@ fst x
          | Error e -> Lwt.fail_with e) in
  let summary_state_lwt =
    summary#thread
    >|= fun w ->
    React.E.map w#update pids,
    React.E.map (w#set_rate % Option.return) rate in
  let overview_state_lwt =
    overview#thread
    >|= fun w ->
    React.E.map w#update pids,
    React.E.map (w#set_rate % Option.return) rate in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let summary_cell  = new Cell.t ~span ~widgets:[summary] () in
    let overview_cell = new Cell.t ~span ~widgets:[overview] () in
    let cells =
      [ summary_cell
      ; overview_cell ] in
    new t ~cells () in
  box#set_on_destroy
  @@ Some (fun () ->
         ws_lwt
         >|= (fun ((e1, sock1), (e2, sock2)) ->
             React.E.stop ~strong:true e1;
             React.E.stop ~strong:true e2;
             sock1##close;
             sock2##close)
         |> Lwt.ignore_result;
         summary_state_lwt
         >|= (fun (e1, e2) ->
           React.E.stop ~strong:true e1;
           React.E.stop ~strong:true e2)
         |> Lwt.ignore_result;
         overview_state_lwt
         >|= (fun (e1, e2) ->
           React.E.stop ~strong:true e1;
           React.E.stop ~strong:true e2)
         |> Lwt.ignore_result);
  box#widget

let tables ({ id; _ } as stream : Stream.t) control =
  let overview = Widget_tables_overview.make stream control in
  let sock_lwt =
    overview#thread
    >|= fun w ->
    let rate, rate_sock = Requests.Streams.WS.get_bitrate ~id control in
    let e, sock = Requests.Streams.WS.get_tables ~id  control in
    let e = React.E.map w#update e in
    let rate = React.E.map w#set_rate rate in
    e, sock, rate, rate_sock in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let overview_cell = new Cell.t ~span ~widgets:[overview] () in
    let cells = [overview_cell] in
    new t ~cells () in
  box#set_on_destroy
  @@ Some (fun () ->
         sock_lwt
         >|= (fun (e, sock, rate, rate_sock) ->
             React.E.stop ~strong:true e;
             React.E.stop ~strong:true rate;
             sock##close;
             rate_sock##close)
         |> Lwt.ignore_result);
  box#widget

let t2mi ({ id; _ } as stream : Stream.t) control =
  let sequence = Widget_t2mi_sequence.make stream control in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let sequence_cell = new Cell.t ~span ~widgets:[sequence] () in
    let cells = [sequence_cell] in
    new t ~cells () in
  box#widget

let tabs (stream:Stream.t) control =
  let base =
    [ "Лог", "log", (fun () -> errors stream control)
    ; "Сервисы", "services", (fun () -> services stream control)
    ; "PIDs", "pids", (fun () -> pids stream control)
    ; "Таблицы", "tables", (fun () -> tables stream control)
    ; "Битрейт", "bitrate", dummy_tab
    ; "Джиттер", "jitter", dummy_tab
    ; "Архив", "archive", dummy_tab
    ] in
  match stream.typ with
  | T2MI ->
     List.insert_at_idx 4
       ("T2-MI", "t2mi", (fun () -> t2mi stream control)) base
  | TS   -> base

let make_tabs (stream : Stream.t) control =
  List.map (fun (name, hash, f) ->
      new Tab.t ~value:(hash, f) ~content:(Text name) ())
    (tabs stream control)

let make (stream:Stream.t) control =
  new Ui_templates.Page.t (`Dynamic (make_tabs stream control)) ()
