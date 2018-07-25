open Containers
open Components
open Api_js.Requests
open Lwt.Infix
open Common

let dummy_tab = fun () ->
  let div = Dom_html.createDiv Dom_html.document in
  Widget.create div

let tabs (stream:Stream.t) =
  let base = 
    [ "Ошибки",  dummy_tab
    ; "Сервисы", dummy_tab
    ; "Таблицы", dummy_tab
    ; "PIDs",    dummy_tab
    ; "Джиттер", dummy_tab
    ] in
  match stream.typ with
  | `T2mi -> List.insert_at_idx 4 ("T2-MI", dummy_tab) base
  | `Ts   -> base

let make_tabs (stream:Stream.t) =
  let open Tabs in
  List.map (fun (name, f) ->
      { content  = `Text name
      ; disabled = false
      ; href     = None
      ; value    = f }) (tabs stream)

let make (stream:Stream.t) =
  new Ui_templates.Page.t (`Dynamic (make_tabs stream)) ()
