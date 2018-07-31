open Containers
open Components
open Api_js.Requests
open Lwt_result.Infix
open Common

let dummy_tab = fun () ->
  let div = Dom_html.createDiv Dom_html.document in
  Widget.create div

let services (stream:Stream.t) control =
  let open Widget_services_overview in
  let w = make ~config:{ stream } control in
  w#widget

let pids (stream:Stream.t) control =
  let open Widget_pids in
  let w = make ~config:{ stream } control in
  w#widget

let tables (stream:Stream.t) control =
  let open Widget_tables_overview in
  let w = make ~config:{ stream } control in
  w#widget

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
  let open Tabs in
  List.map (fun (name, f) ->
      { content  = `Text name
      ; disabled = false
      ; href     = None
      ; value    = f }) (tabs stream control)

let make (stream:Stream.t) control =
  new Ui_templates.Page.t (`Dynamic (make_tabs stream control)) ()
