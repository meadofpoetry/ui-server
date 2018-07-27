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
  let br_fmt  = Table.(Option (Float None, "-")) in
  let pct_fmt = Table.(Option (Float (Some (Printf.sprintf "%.2f")), "-")) in
  let fmt =
    let open Table in
    let open Format in
    let to_column = to_column ~sortable:true in
    (   to_column "PID",             Int)
    :: (to_column "Тип",             String)
    :: (to_column "Сервис",          Option (String, ""))
    :: (to_column "Битрейт, Мбит/с", br_fmt)
    :: (to_column "%",               pct_fmt)
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
    table#add_row (pid.pid :: pid_type :: pid.service
                   :: None :: None :: None :: None :: []) in
  List.iter add_row init;
  let _ =
    React.E.map (fun (bitrate:bitrate) ->
        List.fold_left (fun rows (pid, br) ->
            let open Table in
            match List.find_opt (fun (row:'a Row.t) ->
                      let cell = match row#cells with a :: _ -> a in
                      cell#value = pid) rows with
            | Some x ->
               let cur, per, min, max =
                 match x#cells with
                 | _ :: _ :: _ :: a :: b :: c :: d :: _ ->
                    a, b, c, d in
               let pct = 100. *. (float_of_int br)
                         /. (float_of_int bitrate.total) in
               let br = (float_of_int br) /. 1_000_000. in
               cur#set_value @@ Some br;
               per#set_value @@ Some pct;
               (match min#value with
                | None -> min#set_value (Some br)
                | Some v -> if br <. v then min#set_value (Some br));
               (match max#value with
                | None -> max#set_value (Some br)
                | Some v -> if br >. v then max#set_value (Some br));
               List.remove ~eq:Equal.physical ~x rows
            | None   -> rows) table#rows bitrate.pids |> ignore;
        bitrate) bitrate in
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


