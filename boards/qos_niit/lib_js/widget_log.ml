open Containers
open Components
open Common
open Board_types
open Lwt_result.Infix

type config = unit [@@deriving yojson]

let name = "Журнал"

let settings = None

let find_stream (streams:Streams.TS.archived_list)
      (stream, (error:Errors.t)) =
  let time = error.timestamp in
  let is_in_range (x:Time.t) (from, till) =
    let e = Time.is_earlier ~than:from x in
    let l = Time.is_later ~than:till x in
    not e && not l in
  List.find_map (fun (x:Streams.TS.archived_item) ->
      if is_in_range time x.period
      then List.find_opt (fun (s:Stream.t) ->
               match s.id with
               | `Ts id -> Stream.equal_id id stream
               | _ -> false) x.streams
      else None) streams

let make_table
      (streams:Streams.TS.archived_list Api_js.Api_types.raw)
      (errors:Errors.raw Api_js.Api_types.raw) =
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let show_time = Time.to_human_string ?tz_offset_s in
  let time = Table.({ to_string  = show_time
                    ; compare    = String.compare
                    ; is_numeric = false }) in
  let fmt  = Table.((to_column ~sortable:true "Время",     Custom time)
                    :: (to_column ~sortable:true "Поток",    (Option (String, "")))
                    :: (to_column ~sortable:true "Число ошибок", Int)
                    (* :: (to_column ~sortable:true "Service",  String) *)
                    :: (to_column ~sortable:true "PID",      Int)
                    (* :: (to_column ~sortable:true "Severity", Option (String,"")) *)
                    :: (to_column ~sortable:true "Событие",    String)
                    :: (to_column "Подробности",                 String)
                    :: []) in
  let table = new Table.t ~fmt () in
  let add_row (stream, (error:Errors.t)) =
    let stream = find_stream streams.data (stream, error)
                 |> Option.flat_map (fun (x:Stream.t) -> x.description) in
    let date  = error.timestamp in
    let pid   = error.pid in
    let check = let num, name = Ts_error.to_name error in
                num ^ " " ^ name in
    let msg   = Ts_error.Description.of_ts_error error in
    table#add_row date stream error.count pid check msg in
  List.iter add_row @@ List.rev errors.data;
  table

let l2 t1 t2 f =
  t1 >>= (fun x1 -> t2 >|= fun x2 -> f x1 x2)

let l3 t1 t2 t3 f =
  l2 t1 t2 (fun x y -> x, y)
  >>= (fun (x1, x2) -> t3 >|= fun x3 -> f x1 x2 x3)

let make
      (errors: Errors.t list React.event)
      (config: config option)
      control =
  let now     = Time.Clock.now_s () in
  let from    =
    Option.get_exn @@ Time.sub_span now (Time.Span.of_int_s 3600) in
  let streams =
    Requests.Streams.HTTP.TS.Archive.get_streams
      ~from ~till:now control
    >>= (function
         | Raw s -> Lwt_result.return s
         | _     -> Lwt.fail_with "got compressed") in
  let errors  =
    Requests.Errors.HTTP.TS.Archive.get_errors
      ~from ~till:now control
    >>= (function Raw s -> Lwt_result.return s
                | _     -> Lwt.fail_with "got compressed") in
  let loader =
    l2 streams errors (fun x y -> x, y)
    >>= (fun (s, e) -> Lwt_result.return (make_table s e))
    >|= Widget.coerce
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    |> Ui_templates.Loader.create_widget_loader
  in
  loader
