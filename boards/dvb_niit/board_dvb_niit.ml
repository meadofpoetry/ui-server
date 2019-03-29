open Application_types
open Board_dvb_types
open Board_dvb_protocol

module Config = Kv_v.RW(Board_settings)

let ( >>= ) = Lwt_result.bind

let invalid_port (src : Logs.src) port =
  let s = (Logs.Src.name src) ^ ": invalid port " ^ (string_of_int port) in
  raise (Boards.Board.Invalid_port s)

let rec has_sync = function
  | [] -> false
  | (_, ({ data; _ } : Measure.t ts)) :: tl ->
     match data.lock, data.bitrate with
     | true, Some x when x > 0 -> true
     | _ -> has_sync tl

let create_logger (b : Topology.topo_board) =
  let log_name = Boards.Board.log_name b in
  let log_src = Logs.Src.create log_name in
  match b.logs with
  | None -> Ok log_src
  | Some x ->
     match Logs.level_of_string x with
     | Ok x ->
        Logs.Src.set_level log_src x;
        Ok log_src
     | Error _ -> Error (`Unknown_log_level x)

let parse_source_id (b : Topology.topo_board) =
  match b.sources with
  | None -> Error `Source_id_not_found
  | Some i ->
     match Util_json.Int.of_yojson i with
     | Ok i -> Ok i
     | Error _ -> Error `Invalid_source_id

let create (b : Topology.topo_board) _ convert_streams
      (send : Cstruct.t -> unit Lwt.t)
      (kv : Kv.RW.t)
      (step : float) =
  Config.create ~default:Board_settings.default kv ["board"; (string_of_int b.control)]
  >>= fun (cfg : Device.config Kv_v.rw) -> Lwt.return @@ create_logger b
  >>= fun (log_src : Logs.src) -> Lwt.return @@ parse_source_id b
  >>= fun (source_id : int) ->
  let (api : Protocol.api) =
    Protocol.create log_src send (fun x -> convert_streams x b) source_id cfg step in
  (* let db = Result.get_exn @@ Db.Conn.create db_conf b.control in
   * let handlers = Board_api.handlers b.control db api events in
   * React.E.(keep @@ map_s (Db.Measurements.insert db) api.notifs.measures); *)
  let state = object
      method finalize () = Lwt.return ()
    end in
  Lwt.return_ok
  @@ Boards.Board.(
    { http = []
    ; ws = []
    ; templates = []
    ; control = b.control
    ; streams_signal = api.notifs.streams
    ; log_source = (fun _ -> React.E.never) (* TODO implement source *)
    ; step = api.loop
    ; connection = api.notifs.state
    ; ports_sync =
        List.fold_left (fun acc (p : Topology.topo_port) ->
            (match p.port with
             | 0 -> React.E.map has_sync api.notifs.measures
                    |> React.S.hold ~eq:(=) false
             | x -> invalid_port log_src x)
            |> fun x -> Ports.add p.port x acc)
          Ports.empty b.ports
    ; ports_active =
        List.fold_left (fun acc (p : Topology.topo_port) ->
            (match p.port with
             | 0 -> React.S.const true
             | x -> invalid_port log_src x)
            |> fun x -> Boards.Board.Ports.add p.port x acc)
          Ports.empty b.ports
    ; stream_handler = None
    ; state = (state :> < finalize : unit -> unit Lwt.t >) })
