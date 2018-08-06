open Containers
open Components
open Api_js.Requests
open Lwt_result.Infix
open Common

let dummy_tab = fun () ->
  let div = Dom_html.createDiv Dom_html.document in
  Widget.create div

let services (stream:Stream.t) control =
  let overview = Widget_services_overview.make ~config:{ stream } control in
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

let pids (stream:Stream.t) control =
  let id = match stream.id with
    | `Ts id -> id
    | _      -> failwith "bad id" in
  let init =
    Requests.Streams.HTTP.get_pids ~id ~limit:1 control
    >>= (function
         | Raw s ->
            (match List.head_opt s.data with
             | Some (_, pids) -> pids.pids
             | None -> [])
            |> Lwt_result.return
         | _     -> Lwt.fail_with "got compressed")
    |> Lwt_result.map_err Api_js.Requests.err_to_string in
  let bitrate, sock = Requests.Streams.WS.get_bitrate ~id control in
  let summary =
    Widget_pids_summary.make
      ~config:{ stream }
      init bitrate
      control in
  let overview =
    Widget_pids_overview.make
      ~config:{ stream }
      init bitrate
      control in
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
    [ "Ошибки",  dummy_tab
    ; "Сервисы", (fun () -> services stream control)
    ; "PIDs",    (fun () -> pids stream control)
    ; "Таблицы", (fun () -> tables stream control)
    ] in
  match stream.typ with
  | `T2mi -> List.insert_at_idx 4 ("T2-MI", dummy_tab) base
  | `Ts   -> base

let make_tabs (stream:Stream.t) control =
  List.map (fun (name, f) ->
      new Tab.t ~value:f ~content:(Text name) ())
    (tabs stream control)

let make (stream:Stream.t) control =
  new Ui_templates.Page.t (`Dynamic (make_tabs stream control)) ()
