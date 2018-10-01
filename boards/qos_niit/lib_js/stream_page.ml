open Containers
open Components
open Api_js.Requests
open Lwt_result.Infix
open Common
open Board_types

let ( >>* ) x f = Lwt_result.map_err f x
let ( % ) = Fun.( % )

let dummy_tab = fun () ->
  Ui_templates.Placeholder.under_development ()

let errors ({ id; _ } as stream : Stream.t) control =
  let overview = Widget_errors_log.make stream control in
  let sock_lwt =
    overview#thread
    >|= fun w ->
    let e, sock = Requests.Streams.WS.get_errors ~ids:[id] control in
    let e = React.E.map (
                List.iter (fun (_, x) ->
                    List.iter (ignore % w#prepend_error) x)) e in
    e, sock in
  let box, dialog_lwt =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let icon = Icon.SVG.(create_simple Path.settings) in
    let button = new Icon_button.t ~disabled:true ~icon () in
    let overview_cell = new Cell.t ~span ~widgets:[overview] () in
    let cells = [overview_cell] in
    let grid = new t ~cells () in
    let t =
      overview#thread
      >|= (fun w ->
        button#set_disabled false;
        let settings = w#settings_widget in
        let dialog =
          let open Dialog in
          new t
            ~title:"Настройки отображения"
            ~content:(`Widgets [settings])
            ~actions:[ new Action.t ~typ:`Cancel ~label:"Отмена" ()
                     ; new Action.t ~typ:`Accept ~label:"Применить" () ]
            () in
        Dom.appendChild Dom_html.document##.body dialog#root;
        button#listen_click_lwt (fun _ _ ->
            Lwt.(settings#reset ();
                 dialog#show_await ()
                 >|= function `Accept -> settings#apply ()
                            | `Cancel -> ()))
        |> Lwt.ignore_result;
        let title = new Text.t ~text:"Журнал ошибок" () in
        let title_box =
          new Hbox.t
            ~halign:`Space_between
            ~valign:`Center
            ~widgets:[title#widget; button#widget]
            () in
        let title_cell = new Cell.t ~span ~widgets:[title_box] () in
        grid#insert_cell_at_idx 0 title_cell;
        dialog) in
    (grid, t) in
  box#set_on_destroy
  @@ Some (fun () ->
         dialog_lwt
         >|= (fun dialog ->
             Dom.removeChild Dom_html.document##.body dialog#root)
         |> Lwt.ignore_result;
         sock_lwt
         >|= (fun (e, sock) ->
             React.E.stop ~strong:true e;
             sock##close)
         |> Lwt.ignore_result);
  box#widget

let services ({ id; _ } as stream : Stream.t) control =
  let overview = Widget_services_overview.make stream control in
  let sock_lwt =
    let open React in
    overview#thread
    >|= fun w ->
    let rate, rate_sock = Requests.Streams.WS.get_bitrate ~ids:[id] control in
    let pids, pids_sock = Requests.Streams.WS.get_pids ~ids:[id] control in
    let e, sock = Requests.Streams.WS.get_services ~ids:[id]  control in
    let e = E.map (function [(_, x)] -> w#update x | _ -> ()) e in
    let pids = E.map (function [(_, x)] -> w#update_pids x | _ -> ()) pids in
    let rate =
      E.map (function
          | [(_, (x : Bitrate.t timestamped))] ->
             w#set_rate @@ Option.return x.data
          | _ -> ()) rate in
    e, sock, pids, pids_sock, rate, rate_sock in
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
  let init = Requests.Streams.HTTP.get_pids ~ids:[id] control
             |> Lwt_result.map_err Api_js.Requests.err_to_string in
  let summary = Widget_pids_summary.make ~init stream control in
  let overview = Widget_pids_overview.make ~init stream control in
  let ws_lwt =
    Lwt.choose [ summary#thread >|= Widget.coerce
               ; overview#thread >|= Widget.coerce ]
    >|= fun _ ->
    (Requests.Streams.WS.get_bitrate ~ids:[id] control),
    (Requests.Streams.WS.get_pids ~ids:[id] control) in
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
    let open React in
    summary#thread
    >|= fun w ->
    let pids = E.map (function [(_, x)] -> w#update x | _ -> ()) pids in
    let rate =
      E.map (function
          | [(_, (x : Bitrate.t timestamped))] ->
             w#set_rate @@ Option.return x.data
          | _ -> ()) rate in
    pids, rate in
  let overview_state_lwt =
    let open React in
    overview#thread
    >|= fun w ->
    let pids = E.map (function [(_, x)] -> w#update x | _ -> ()) pids in
    let rate =
      E.map (function
          | [(_, (x : Bitrate.t timestamped))] ->
             w#set_rate @@ Option.return x.data
          | _ -> ()) rate in
    pids, rate in
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
  let open React in
  let overview = Widget_tables_overview.make stream control in
  let sock_lwt =
    overview#thread
    >|= fun w ->
    let rate, rate_sock = Requests.Streams.WS.get_bitrate ~ids:[id] control in
    let e, sock = Requests.Streams.WS.get_tables ~ids:[id] control in
    let e = E.map (function [(_, x)] -> w#update x | _ -> ()) e in
    let rate =
      E.map (function
          | [(_, (x : Bitrate.t timestamped))] ->
             w#set_rate x.data
          | _ -> ()) rate in
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
    (* ; "Битрейт", "bitrate", dummy_tab
     * ; "Джиттер", "jitter", dummy_tab
     * ; "Архив", "archive", dummy_tab *)
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
