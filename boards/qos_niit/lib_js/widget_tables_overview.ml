open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types

type config =
  { stream : Stream.t
  }

let name = "Обзор таблиц"

let settings = None

let make_table (init:table_info list) =
  let fmt =
    let open Table in
    let open Format in
    (   to_column ~sortable:true "PID",     Int)
    :: (to_column ~sortable:true "ID",      Int)
    :: (to_column ~sortable:true "Имя",     String)
    :: (to_column "Extra",                  Option (String, ""))
    :: (to_column ~sortable:true "Версия",  Int)
    :: (to_column ~sortable:true "Сервис",  Option (String, ""))
    :: (to_column "Section",                Int)
    :: (to_column "Last section",           Int)
    :: [] in
  let table = new Table.t ~dense:true ~fmt () in
  let add_row (x:table_info) =
    table#add_row (x.pid :: x.id
                   :: (table_to_string @@ table_of_int x.id)
                   :: None :: x.version :: None
                   :: x.section :: x.last_section :: []) in
  List.iter add_row init;
  table

let make ~(config:config) control =
  let id   = match config.stream.id with
    | `Ts id -> id
    | `Ip _  -> failwith "UDP" in
  let init =
    Requests.Streams.HTTP.get_tables ~id ~limit:1 control
    >>= (function
         | Raw s -> Lwt_result.return s.data
         | _     -> Lwt.fail_with "got compressed") in
  let loader =
    init
    >|= (fun init ->
      let tables = match List.head_opt init with
        | Some (_, tables) -> tables.tables
        | None -> [] in
      make_table tables)
    >|= Widget.coerce
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    |> Ui_templates.Loader.create_widget_loader
  in loader


