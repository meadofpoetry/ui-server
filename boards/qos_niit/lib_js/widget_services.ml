open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types

type config =
  { stream : Stream.id
  }

let default_config =
  { stream = Single
  }

let name = "Сервисы"

let settings = None

let make_table (init:service_info list) =
  let fmt =
    let open Table in
    let open Format in
    (to_column "ID", Int None)
    :: (to_column "Имя", String None)
    :: (to_column "Битрейт", Option (String None, ""))
    :: [] in
  let table = new Table.t ~dense:true ~fmt () in
  let add_row (service:service_info) =
    table#add_row (service.id :: service.name :: None :: []) in
  List.iter Fun.(ignore % add_row) init;
  table

let make ?(config=default_config)
      control =
  let id   = config.stream in
  let init =
    Requests.Streams.HTTP.get_services ~id ~limit:1 control
    >>= (function
         | Raw s -> Lwt_result.return s.data
         | _     -> Lwt.fail_with "got compressed") in
  let loader =
    init
    >|= (fun init ->
      let services = match List.head_opt init with
        | Some (_, services) -> services.services
        | None -> [] in
      make_table services)
    >|= Widget.coerce
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    |> Ui_templates.Loader.create_widget_loader
  in loader


