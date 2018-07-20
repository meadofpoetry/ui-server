open Containers
open Components
open Api_js.Requests
open Lwt.Infix
open Common

let dummy_tab = fun () ->
  let div = Dom_html.createDiv Dom_html.document in
  Widget.create div

let tabs =
  [ "Ошибки",  dummy_tab
  ; "Сервисы", dummy_tab
  ; "Таблицы", dummy_tab
  ; "PIDs",    dummy_tab
  ; "T2-MI",   dummy_tab (* FIXME should be optional or disabled/enabled *)
  ; "Джиттер", dummy_tab
  ]

