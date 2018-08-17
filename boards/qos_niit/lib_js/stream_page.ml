open Containers
open Components
open Api_js.Requests
open Lwt_result.Infix
open Common
open Board_types.Streams.TS

let ( >>* ) x f = Lwt_result.map_err f x

let get_pids ~id control =
  Requests.Streams.HTTP.get_pids ~id ~limit:1 control
  >>= (function
       | Raw s ->
          (match List.head_opt s.data with
           | Some (_, pids) -> pids.pids
           | None -> [])
          |> Lwt_result.return
       | _     -> Lwt.fail_with "got compressed")
  |> Lwt_result.map_err Api_js.Requests.err_to_string

let get_services ~id control =
  Requests.Streams.HTTP.get_services ~id ~limit:1 control
  >>* Api_js.Requests.err_to_string
  >>= function
  | Raw s ->
     begin match List.head_opt s.data with
     | Some (_, services) -> services.services
     | None -> []
     end
     |> Lwt_result.return
  | _ -> Lwt.fail_with "got compressed"

let get_errors ?from ?till ?duration ?limit ~id control =
  let open Requests.Streams.HTTP.Errors in
  get_errors ?limit ?from ?till ?duration ~id control
  >>* Api_js.Requests.err_to_string
  >>= function
  | Raw s -> Lwt_result.return s.data
  | _ -> Lwt.fail_with "got compressed"

let dummy_tab = fun () ->
  Ui_templates.Placeholder.under_development ()

let errors ({ id; _}:Stream.t) control =
  let e, sock = Requests.Streams.WS.Errors.get_errors ~id control in
  let overview =
    get_errors ~limit:20 ~id control
    >|= Widget_errors_log.make
    >|= (fun w ->
      (* FIXME save event *)
      let _ = React.E.map (List.map w#add_error) e in
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

let services ({ id; _ }:Stream.t) control =
  let e_br, br_sock = Requests.Streams.WS.get_bitrate ~id control in
  let overview =
    get_pids ~id control
    >>= (fun pids -> get_services ~id control >|= fun x -> x, pids)
    >|= (fun (svs, pids) -> Widget_services_overview.make svs pids e_br)
    >|= Widget.coerce
    |> Ui_templates.Loader.create_widget_loader in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let overview_cell = new Cell.t ~span ~widgets:[ overview ] () in
    let cells =
      [ new Cell.t ~span ~widgets:[new Text.t ~text:"Обзор" ()] ()
      ; overview_cell ] in
    new t ~cells () in
  box#widget

let pids ({ id; _ } as stream : Stream.t) control =
  let bitrate, sock = Requests.Streams.WS.get_bitrate ~id control in
  let init = get_pids ~id control in
  let summary =
    Widget_pids_summary.make
      ~config:{ stream }
      init bitrate
      control in
  let overview =
    init
    >|= (fun init ->
      let w = Widget_pids_overview.make init in
      let _e = React.E.map (fun (x:bitrate) ->
                   w#set_rate @@ Some (x.total, x.pids)) bitrate in
      w#set_on_destroy
      @@ Some (fun () -> React.E.stop ~strong:true _e;
                         React.E.stop ~strong:true bitrate;
                         sock##close);
      w)
    >|= Widget.coerce
    |> Ui_templates.Loader.create_widget_loader in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let summary_cell  = new Cell.t ~span ~widgets:[ summary ] () in
    let overview_cell = new Cell.t ~span ~widgets:[ overview ] () in
    let cells =
      [ new Cell.t ~span ~widgets:[new Text.t ~text:"Краткая сводка" ()] ()
      ; summary_cell
      ; new Cell.t ~span ~widgets:[new Text.t ~text:"Обзор" ()] ()
      ; overview_cell ] in
    new t ~cells () in
  box#widget

let tables (stream:Stream.t) control =
  let overview = Widget_tables_overview.make ~config:{ stream } control in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let overview_cell = new Cell.t ~span ~widgets:[ overview ] () in
    let cells =
      [ new Cell.t ~span ~widgets:[new Text.t ~text:"Обзор" ()] ()
      ; overview_cell ] in
    new t ~cells () in
  box#widget

let tabs (stream:Stream.t) control =
  let base =
    [ "Ошибки",  (fun () -> errors stream control)
    ; "Сервисы", (fun () -> services stream control)
    ; "PIDs",    (fun () -> pids stream control)
    ; "Таблицы", (fun () -> tables stream control)
    ; "Битрейт", dummy_tab
    ; "Джиттер", dummy_tab
    ; "Архив",   dummy_tab
    ] in
  match stream.typ with
  | T2MI -> List.insert_at_idx 4 ("T2-MI", dummy_tab) base
  | TS   -> base

let make_tabs (stream:Stream.t) control =
  List.map (fun (name, f) ->
      new Tab.t ~value:f ~content:(Text name) ())
    (tabs stream control)

let make (stream:Stream.t) control =
  new Ui_templates.Page.t (`Dynamic (make_tabs stream control)) ()
