
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

let name = "PIDs"

let settings = None

let make_table (init:pid_info list)
      (bitrate:bitrate React.event)=
  let br_fmt = Table.(Option (Float None, "")) in
  let fmt =
    let open Table in
    (   to_column "PID",             Int)
    :: (to_column "Тип",             String)
    :: (to_column "Сервис",          Option (String, ""))
    :: (to_column "Битрейт, Мбит/с", br_fmt)
    :: (to_column "Min, Мбит/с",     br_fmt)
    :: (to_column "Max, Мбит/с",     br_fmt)
    :: [] in
  let table = new Table.t ~dense:true ~fmt () in
  let add_row (pid:pid_info) =
    let pid_type = match pid.pid_type with
      | SEC l   ->
         let s = List.map Fun.(table_to_string % table_of_int) l
                 |> String.concat ", " in
         "SEC -> " ^ s
      | PES x   ->
         let s = Mpeg_ts.stream_type_to_string x in
         "PES -> " ^ s
      | ECM x   -> "ECM -> " ^ (string_of_int x)
      | EMM x   -> "EMM -> " ^ (string_of_int x)
      | Null    -> "Null"
      | Private -> "Private" in
    table#add_row pid.pid pid_type pid.service
      None None None in
  List.iter add_row init;
  let _ =
    React.E.map (fun (br:bitrate) ->
        List.fold_left (fun rows (pid, br) ->
            match List.find_opt (fun (row:('a,'b) Table.Row.t) ->
                      let cell = List.get_at_idx_exn 0 row#cells_widgets in
                      int_of_string (Option.get_exn cell#text_content) = pid) rows with
            | Some x ->
               let br = float_of_int br /. 1_000_000. in
               let f  = x#update in
               f 0 "" None (Some br) None None;
               List.remove ~eq:Equal.physical ~x rows
            | None   -> rows) table#rows br.pids |> ignore;
        br) bitrate in
  table

let make ?(config=default_config)
      control =
  let id   = config.stream in
  let init =
    Requests.Streams.HTTP.get_pids ~id ~limit:1 control
    >>= (function
         | Raw s -> Lwt_result.return s.data
         | _     -> Lwt.fail_with "got compressed") in
  let loader =
    init
    >|= (fun init ->
      let e, sock = Requests.Streams.WS.get_bitrate ~id control in
      let pids = match List.head_opt init with
        | Some (_, pids) -> pids.pids
        | None -> [] in
      make_table pids e)
    >|= Widget.coerce
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    |> Ui_templates.Loader.create_widget_loader
  in loader


