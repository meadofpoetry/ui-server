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
    [ "Лог", "log", dummy_tab
    ; "Сервисы", "services", dummy_tab
    ; "PIDs", "pids", dummy_tab
    ; "Таблицы", "tables", dummy_tab
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
