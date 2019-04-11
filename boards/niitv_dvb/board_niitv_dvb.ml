open Application_types
open Board_niitv_dvb_types
open Board_niitv_dvb_protocol
open Boards

module Config = Kv_v.RW(Board_settings)

let ( >>= ) = Lwt_result.bind

let invalid_port (src : Logs.src) port =
  let s = (Logs.Src.name src) ^ ": invalid port " ^ (string_of_int port) in
  raise (Board.Invalid_port s)

let rec has_sync = function
  | [] -> false
  | (_, ({ data; _ } : Measure.t ts)) :: tl ->
     match data.lock, data.bitrate with
     | true, Some x when x > 0 -> true
     | _ -> has_sync tl

let create_logger (b : Topology.topo_board) =
  let log_name = Board.log_name b in
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
  | None -> Error (`Board_error "Source id not found")
  | Some i ->
     match Util_json.Int.of_yojson i with
     | Ok i -> Ok i
     | Error _ -> Error (`Board_error "Invalid source id")

let create ({ control; _ } as b : Topology.topo_board)
      (_ : Stream.t list React.signal)
      (convert_streams : Topology.topo_board ->
                         Stream.Raw.t list React.signal ->
                         Stream.t list React.signal)
      (send : Cstruct.t -> unit Lwt.t)
      (db : Db.t)
      (kv : Kv.RW.t) : (Board.t, [> Board.error]) Lwt_result.t =
  Config.create ~default:Board_settings.default kv ["board"; (string_of_int b.control)]
  >>= fun (cfg : Device.config Kv_v.rw) -> Lwt.return (create_logger b)
  >>= fun (src : Logs.src) -> Lwt.return (parse_source_id b)
  >>= fun (source_id : int) ->
  Protocol.create src send (convert_streams b) source_id cfg control db
  >>= fun (api : Protocol.api) ->
  let state = object
      method finalize () = Lwt.return ()
    end in
  let (board : Board.t) =
    { http = Board_niitv_dvb_http.handlers b.control api
    ; ws = Board_niitv_dvb_http.ws b.control api
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
             | x -> invalid_port src x)
            |> fun x -> Board.Ports.add p.port x acc)
          Board.Ports.empty b.ports
    ; ports_active =
        List.fold_left (fun acc (p : Topology.topo_port) ->
            (match p.port with
             | 0 -> React.S.const true
             | x -> invalid_port src x)
            |> fun x -> Board.Ports.add p.port x acc)
          Board.Ports.empty b.ports
    ; stream_handler = None
    ; state = (state :> < finalize : unit -> unit Lwt.t >)
    } in
  Lwt.return_ok board
