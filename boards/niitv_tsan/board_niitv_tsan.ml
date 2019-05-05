open Boards
open Board_qos_types
open Containers
open Lwt.Infix
open Common

let ( % ) = Fun.( % )

module Api_handler = Api.Handler.Make (User)

module Data = struct

  type t = config
  let default =
    { input = ASI
    ; t2mi_mode = None
    ; jitter_mode = None
    }
  let dump c = Yojson.Safe.to_string @@ config_to_yojson c
  let restore s = config_of_yojson @@ Yojson.Safe.from_string s

end

module Config_storage = Storage.Options.Make (Data)

let tick tm =
  let e, push = React.E.create () in
  let rec loop () =
    push (); Lwt_unix.sleep tm >>= loop
  in
  e, loop

let appeared_streams
    sources
    ~(past : Stream.t list)
    ~(pres : Stream.t list) =
  let open Common.Stream in
  let rec not_in_or_diff s = function
    | [] -> true
    | so :: _ when equal so s -> false
    | _ :: tl -> not_in_or_diff s tl
  in
  let appeared =
    List.fold_left (fun acc pres ->
        if not_in_or_diff pres past
        then (Board_protocol.is_incoming sources pres, pres) :: acc
        else acc) [] pres in
  appeared

let create (b : Topology.topo_board) _ convert_streams send
    db_conf base step : Board.t =
  let open Db.Data_handler in
  let log_name = Boards.Board.log_name b in
  let log_src = Logs.Src.create log_name in
  Option.iter (fun x -> Logs.Src.set_level log_src @@ Some x) b.logs;
  let (module Logs : Logs.LOG) = Logs.src_log log_src in
  let module SM = Board_protocol.Make(Logs) in
  let module Logger = Board_logger.Make(Logs) in
  let module Ports = Board_ports.Make(Logs) in
  let sources = match b.sources with
    | None ->
      let s = log_name ^ ": no sources provided!" in
      raise (Board.Invalid_sources s)
    | Some x ->
      begin match init_of_yojson x with
        | Ok init -> init
        | Error s -> raise (Board.Invalid_sources s)
      end in
  let conv = fun x -> convert_streams x b in
  let storage =
    Config_storage.create base
      ["board"; (string_of_int b.control)] in
  let ({ ts; t2mi; _ } as events), api, step =
    SM.create sources send storage step conv in
  let db = Result.get_exn @@ Db.Conn.create db_conf b.control in
  let handlers = Board_api.handlers b.control db sources api events in
  let tick, tick_loop = tick 5. in
  let open React in
  (* State *)
  Lwt.ignore_result @@ Db.Device.init db;
  E.keep
  @@ E.map_p (fun e -> Db.Device.bump db e)
  @@ E.select [ S.changes events.device.state
              ; S.sample (fun _ e -> e) tick events.device.state ];
  (* Streams *)
  let streams_ev =
    S.sample (fun () sl -> `Active sl) tick events.streams in
  let streams_diff =
    S.diff (fun pres past -> `New (appeared_streams sources ~past ~pres))
      events.streams in
  E.(keep
     @@ map_s (function
         | `Active x -> Db.Streams.bump_streams db x
         | `New x -> Db.Streams.insert_streams db x)
     @@ select [streams_ev; streams_diff]);
  Db.Ts_info.(Single.handle ~eq:Ts_info.equal ~insert ~bump db tick ts.info);
  Db.Pids.(Coll.handle ~eq:Pid.equal ~insert ~bump db tick ts.pids);
  Db.Services.(Coll.handle ~eq:Service.equal ~insert ~bump db tick ts.services);
  Db.T2mi_info.(Coll.handle ~eq:T2mi_info.equal ~insert ~bump db tick t2mi.structures);
  E.(keep @@ map_s (Db.Bitrate.insert db) ts.bitrates);
  E.(keep @@ map_s (Db.Bitrate.insert_pids db) ts.bitrates);
  (* E.(keep @@ map_p (Db.Errors.insert ~is_ts:true  db) events.ts.errors); *)
  E.(keep @@ map_p (Db.Errors.insert ~is_ts:false db) events.t2mi.errors);
  let ports_sync = Ports.sync b events in
  let ports_active = Ports.active b events in
  let state = (object
    val events = events
    val db = db
    val _tick = tick_loop ()
    method finalize () = ()
  end) in (* TODO fix finalize *)
  { handlers = handlers
  ; control = b.control
  ; streams_signal = events.streams
  ; log_source = Logger.make_event b.control events
  ; step
  ; connection = events.device.state
  ; ports_sync
  ; ports_active
  ; stream_handler = None
  ; state = (state :> < finalize : unit -> unit >)
  ; templates = None
  }
