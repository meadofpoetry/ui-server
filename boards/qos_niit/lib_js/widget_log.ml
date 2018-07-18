open Containers
open Components
open Common
open Board_types
open Lwt_result.Infix
open Api_js.Api_types

type config = unit [@@deriving yojson]

let name = "Журнал"

let settings = None

let rec fetch_all acc req from till f =
  let t = req from till in
  t >>= (function
         | (Raw x) ->
            (match List.head_opt x.data with
             | Some tl ->
                if x.has_more
                then let from = f tl in
                     fetch_all (acc @ x.data) req from till f
                else Lwt_result.return (acc @ x.data)
             | None -> Lwt_result.return @@ acc @ x.data)
         | _ -> assert false)

let find_service (error:Errors.t)
      (structure:Streams.TS.structure) =
  let open Streams.TS in
  let pid = error.pid in
  List.find_opt (fun (x:service_info) ->
      List.mem ~eq:(=) pid @@ List.map (fun (x:es_info) -> x.pid) x.es)
    structure.services

let find_structure (stream:Stream.id)
      (error:Errors.t)
      (structures:(Stream.id * Streams.TS.structure) list) =
  let open Streams.TS in
  let s = List.filter_map (fun (id,s) ->
              if Stream.equal_id id stream
              then Some s else None) structures
          |> List.sort (fun (x:structure) y ->
                 Time.compare x.timestamp y.timestamp) in
  List.fold_while (fun acc x ->
      if Time.is_earlier x.timestamp ~than:error.timestamp
         || Time.equal x.timestamp error.timestamp
      then (Some x), `Continue
      else acc, `Stop) None s

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
               | _      -> false) x.streams
      else None) streams

let make_table
      (streams:Streams.TS.archived_list Api_js.Api_types.raw)
      (structures:(Stream.id * Streams.TS.structure) list)
      (errors:Errors.raw Api_js.Api_types.raw) =
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let show_time = Time.to_human_string ?tz_offset_s in
  let time = Table.({ to_string  = show_time
                    ; compare    = String.compare
                    ; is_numeric = false }) in
  let fmt  =
    Table.((to_column ~sortable:true "Время",     Custom time)
           :: (to_column ~sortable:true "Поток",    String)
           :: (to_column ~sortable:true "Service",  Option (String, ""))
           :: (to_column ~sortable:true "Число ошибок", Int)
           :: (to_column ~sortable:true "PID",      Int)
           (* :: (to_column ~sortable:true "Severity", Option (String,"")) *)
           :: (to_column ~sortable:true "Событие",    String)
           :: (to_column "Подробности",                 String)
           :: []) in
  let table = new Table.t ~fmt () in
  let add_row (stream, (error:Errors.t)) =
    let default = Stream.show_id stream in
    let service =
      find_structure stream error structures
      |> Option.flat_map (fun x -> find_service error x)
      |> Option.map (fun (x:Streams.TS.service_info) -> x.name) in
    let stream =
      find_stream streams.data (stream, error)
      |> Option.flat_map (fun (x:Stream.t) -> x.description)
      |> Option.get_or ~default in
    let date  = error.timestamp in
    let pid   = error.pid in
    let check = let num, name = Ts_error.to_name error in
                num ^ " " ^ name in
    let msg   = Ts_error.Description.of_ts_error error in
    table#add_row date stream service error.count pid check msg in
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
    Option.get_exn @@ Time.sub_span now (Time.Span.of_int_s 120) in
  let streams =
    let req () =
      Requests.Streams.HTTP.TS.Archive.get_streams
        ~from ~till:now control in
    req ()
    >>= (function
         | Raw s -> Lwt_result.return s
         | _     -> Lwt.fail_with "got compressed") in
  let structures =
    let req from till =
      Requests.Streams.HTTP.TS.Archive.get_structure
        ~from ~till control in
    fetch_all [] req from now (fun (_,(x:Streams.TS.structure)) ->
        x.timestamp) in
  let errors  =
    let req () = Requests.Errors.HTTP.TS.Archive.get_errors
                   ~from ~till:now control in
    req ()
    >>= (function Raw s -> Lwt_result.return s
                | _     -> Lwt.fail_with "got compressed") in
  let loader =
    l3 streams structures errors (fun x y z -> x, y, z)
    >>= (fun (sms, str, e) -> Lwt_result.return (make_table sms str e))
    >|= Widget.coerce
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    |> Ui_templates.Loader.create_widget_loader
  in
  loader
