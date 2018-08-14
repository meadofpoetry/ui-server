open Common.Topology
open Boards.Board
open Board_types
open Containers

module Api_handler = Api.Handler.Make (Common.User)

module Data = struct

  type t      = config
  let default = config_default
  let dump    = config_to_string
  let restore = config_of_string

end

module Config_storage = Storage.Options.Make (Data)

let log_prefix control = Printf.sprintf "(Board QoS: %d) " control

let tick tm =
  let open Lwt.Infix in
  let e,push = Lwt_react.E.create () in
  let rec loop () =
    push (); Lwt_unix.sleep tm >>= loop
  in
  e, loop

let appeared_streams
      ~(past:Common.Stream.t list)
      ~(pres:Common.Stream.t list) =
  let open Common.Stream in
  let rec not_in_or_diff s = function
    | [] -> true
    | so :: _ when equal so s -> false
    | _ :: tl -> not_in_or_diff s tl
  in
  let appeared = List.fold_left (fun acc pres ->
                     if not_in_or_diff pres past
                     then pres :: acc else acc) [] pres in
  appeared

let invalid_port x =
  let s = "Board_qos_niit: invalid port " ^ (string_of_int x) in
  raise (Invalid_port s)

let get_ports_sync streams input ports =
  let open React in
  List.fold_left (fun acc p ->
      (match p.port with
       | 0 -> S.l2 (fun i s -> match i, s with
                               | SPI, _ :: _ -> true
                               | _ -> false) input streams
       | 1 -> S.l2 (fun i s -> match i, s with
                               | ASI, _ :: _ -> true
                               | _ -> false) input streams
       | x -> invalid_port x)
      |> fun x -> Ports.add p.port x acc) Ports.empty ports

let get_ports_active input ports =
  let open React in
  List.fold_left (fun acc p ->
      (match p.port with
       | 0 -> S.map (function SPI -> true | _ -> false) input
       | 1 -> S.map (function ASI -> true | _ -> false) input
       | x -> invalid_port x)
      |> fun x -> Ports.add p.port x acc) Ports.empty ports

let create (b:topo_board) _ convert_streams send db_conf base step =
  let sources = match b.sources with
    | None -> raise (Invalid_sources "no sources provided!")
    | Some x ->
       begin match Types.init_of_yojson x with
       | Ok init -> init
       | Error s -> raise (Invalid_sources s)
       end in
  let conv    = fun x -> convert_streams x b in
  let storage =
    Config_storage.create base
      ["board"; (string_of_int b.control)] in
  let events,api,step =
    Board_protocol.SM.create sources (log_prefix b.control)
      send storage step conv in
  let db       = Result.get_exn @@ Db.Conn.create db_conf b.control in
  let handlers = Board_api.handlers b.control db api events in
  let tick, tick_loop = tick 5. in
  let open Lwt_react in
  (* State *)
  Lwt.ignore_result @@ Db.Device.init db;
  E.keep
  @@ E.map_p (fun e -> Db.Device.bump db e)
  @@ E.select [ S.changes events.device.state
              ; S.sample (fun _ e -> e) tick events.device.state ];
  (* Streams *)
  let () = S.map (fun s ->
               List.map Common.Stream.show s
               |> String.concat "\n"
               |> fun s -> Logs.err (fun m -> m "%s\n" s)) events.streams
           |> Lwt_react.S.keep in
  let streams_ev =
    S.sample (fun () sl -> `Active sl) tick events.streams in
  let streams_diff =
    S.diff (fun pres past -> `New (appeared_streams ~past ~pres))
      events.streams in
  E.(keep
     @@ map_s (function
            | `Active x -> Db.Streams.bump_streams db x
            | `New x    -> Db.Streams.insert_streams db x)
     @@ select [streams_ev; streams_diff]);
  (* Structs ts *)
  E.(keep @@ map_p (Db.Streams.insert_ts_info db) events.ts.info);
  E.(keep @@ map_p (Db.Streams.insert_services db) events.ts.services);
  E.(keep @@ map_p (Db.Streams.insert_tables db) events.ts.tables);
  E.(keep @@ map_p (Db.Streams.insert_pids db) events.ts.pids);
  (* Structs t2 *)
  E.(keep @@ map_p (Db.Streams.insert_t2mi_info db) events.t2mi.structures);
  (* TS bitrates *)
  (* Errors *)
  E.(keep @@ map_p (Db.Errors.insert ~is_ts:true  db) events.ts.errors);
  E.(keep @@ map_p (Db.Errors.insert ~is_ts:false db) events.t2mi.errors);
  let state = (object
                 val db    = db
                 val _tick = tick_loop ()
                 method finalize () = ()
               end) in (* TODO fix finalize *)
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = events.streams
  ; step           = step
  ; connection     = events.device.state
  ; ports_sync     = get_ports_sync events.streams events.device.input b.ports
  ; ports_active   = get_ports_active events.device.input b.ports
  ; stream_handler = None
  ; state          = (state :> < finalize : unit -> unit >)
  }
