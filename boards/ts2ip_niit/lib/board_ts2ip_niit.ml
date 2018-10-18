open Containers
open Boards
open Board_types
open Common

module Data = struct

  type t = Board_types.config
  let default =
    { nw_mode =
        { ip = Ipaddr.V4.make 192 168 100 200
        ; mask = Ipaddr.V4.make 255 255 255 0
        ; gateway = Ipaddr.V4.make 192 168 100 1
        }
    ; factory_mode =
        { mac = Macaddr.of_string_exn "00:50:c2:88:50:ab" }
    ; packers = []
    }
  let dump c = Yojson.Safe.to_string @@ config_to_yojson c
  let restore s = config_of_yojson @@ Yojson.Safe.from_string s

end

module Config_storage = Storage.Options.Make (Data)

let get_ports_sync board streams =
  let open React in
  List.fold_left (fun acc p ->
      S.map ~eq:Equal.bool (fun streams ->
          List.filter_map (Stream.to_topo_port board) streams
          |> List.mem ~eq:Topology.equal_topo_port p) streams
      |> fun x -> Board.Ports.add p.port x acc)
    Board.Ports.empty board.ports

let make_streams (events : Board_protocol.events) =
  let eq =
    Pair.equal (Equal.option Url.equal) Stream.equal
    |> Equal.list in
  React.S.l3 ~eq (fun incoming outgoing config ->
      List.map (fun (x : Stream.t) ->
          let (set : Stream.t list) =
            List.map (fun x -> x.stream) config.packers in
          let stream =
            List.find_opt (fun (o : Stream.t) ->
                Stream.ID.equal o.id x.id) outgoing in
          match stream with
          | Some o ->
             begin match o.orig_id with
             | TSoIP uri ->
                let (uri : Url.t) =
                  { ip = uri.addr; port = uri.port } in
                Some uri, x
             | _ -> assert false (* unreachable *)
             end
          | None -> None, x) incoming)
    events.in_streams events.out_streams events.config

let create (b : Topology.topo_board) (streams : Stream.t list React.signal) _
      send _ base step : Board.t =
  let log_name = Boards.Board.log_name b in
  let log_src = Logs.Src.create log_name in
  let () = Option.iter (fun x -> Logs.Src.set_level log_src
                                 @@ Some x) b.logs in
  let logs = Logs.src_log log_src in
  let storage =
    Config_storage.create base ["board"; (string_of_int b.control)] in
  let events, api, step =
    Board_protocol.SM.create logs send storage step streams b in
  let handlers = Board_api.handlers b.control api events in
  let constraints =
    Board.{ state =
              React.S.l2 ~eq:equal_set_state (fun s d ->
                  match s, d with
                  | `Fine, Some devi ->
                     begin match devi.packers_num with
                     | Some x when x > 0 -> `Limited x
                     | Some _ | None -> `Forbidden
                     end
                  | _ -> `Forbidden)
                events.state events.devinfo
          ; range =
              [ { ip = Ipaddr.V4.make 224 1 2 2; port = 1234 },
                { ip = Ipaddr.V4.make 239 255 255 255; port = 65535 }
              ]
    } in
  let set streams =
    match React.S.value events.state with
    | `Fine ->
       (try
          List.map (fun ((url : Url.t), stream) ->
              if List.fold_left (fun acc x ->
                     if not @@ Url.in_range x url
                     then false else acc) true constraints.range
              then { stream
                   ; dst_ip = url.ip
                   ; dst_port = url.port
                   ; enabled = true
                   }
              else failwith "not in range") streams
          |> api.set_packers
          |> Lwt_result.map_err (function
                 | `Limit_exceeded x -> `Limit_exceeded x
                 | `Undefined_limit  -> `Forbidden)
        with _ -> Lwt_result.fail `Not_in_range)
    | _ -> Lwt_result.fail `Forbidden
  in
  let state = (object method finalize () = () end) in
  { handlers
  ; control = b.control
  ; streams_signal = events.out_streams
  ; step
  ; connection = events.state
  ; ports_sync = get_ports_sync b streams
  ; ports_active =
      List.fold_left (fun acc (p : Topology.topo_port) ->
          Board.Ports.add p.port (React.S.const true) acc)
        Board.Ports.empty b.ports
  ; stream_handler =
      Some (object
            method streams = make_streams events
            method set x = set x
            method constraints = constraints
          end)
  ; state = (state :> < finalize : unit -> unit >)
  ; templates = None
  }
