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

let to_table_extra ?(hex=false) (x:table_info) =
  let id = match hex with
    | true  -> Printf.sprintf "0x%02X"
    | false -> Printf.sprintf "%d" in
  let specific = match table_of_int x.id with
    | `PAT   -> Some [ "tsid", x.id_ext ]
    | `PMT   -> Some [ "program", x.id_ext ]
    | `NIT _ -> Some [ "nid", x.id_ext ]
    | `SDT _ -> Some [ "tsid", x.id_ext ]
    | `BAT   -> Some [ "bid", x.id_ext ]
    | `EIT _ -> Some [ "sid", x.id_ext
                     ; "tsid", x.eit_params.ts_id
                     ; "onid", x.eit_params.orig_nw_id ]
    | _      -> None in
  let open Tyxml_js.Html in
  let wrap x =
    List.mapi (fun i (s, v) ->
        let v = id v in
        let v = if i = pred @@ List.length x then v else v ^ ", " in
        span [ span ~a:[ a_class [ Typography.Markup.body2_class ]]
                 [pcdata (s ^ ": ")]
             ; pcdata v ]) x
    |> fun x -> span x in
  (match specific with
   | Some l -> wrap l
   | None   -> span [])
  |> Tyxml_js.Html.toelt

let make_table (init:table_info list) =
  (* FIXME should remember preffered state *)
  let switch = new Switch.t () in
  let dec_pid_fmt = Table.(Int (Some (Printf.sprintf "%d"))) in
  let hex_pid_fmt = Table.(Int (Some (Printf.sprintf "0x%04X"))) in
  let hex_tid_fmt = Table.(Int (Some (Printf.sprintf "0x%02X"))) in
  let fmt =
    let open Table in
    let open Format in
    (   to_column ~sortable:true "PID",     if React.S.value switch#s_state
                                            then hex_pid_fmt else dec_pid_fmt)
    :: (to_column ~sortable:true "ID",      if React.S.value switch#s_state
                                            then hex_tid_fmt else dec_pid_fmt)
    :: (to_column ~sortable:true "Имя",     String None)
    :: (to_column "Extra",                  Html None)
    :: (to_column ~sortable:true "Версия",  Int None)
    :: (to_column ~sortable:true "Сервис",  Option (String None, ""))
    :: (to_column "Section",                Int None)
    :: (to_column "Last section",           Int None)
    :: [] in
  let table = new Table.t ~dense:true ~fmt () in
  let add_row (x:table_info) =
    table#add_row (x.pid :: x.id
                   :: (table_to_string @@ table_of_int x.id)
                   :: (to_table_extra x) :: x.version :: None
                   :: x.section :: x.last_section
                   :: []) in
  List.iter add_row init;
  (* FIXME state *)
  let _ =
    React.E.map (fun x ->
        List.iter (fun row ->
            let open Table in
            match row#cells with
            | pid :: tid :: _ ->
               pid#set_format (if x then hex_pid_fmt else dec_pid_fmt);
               tid#set_format (if x then hex_tid_fmt else dec_pid_fmt))
          table#rows)
    @@ React.S.changes switch#s_state in
  new Vbox.t ~widgets:[ switch#widget
                      ; table#widget ] ()

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


