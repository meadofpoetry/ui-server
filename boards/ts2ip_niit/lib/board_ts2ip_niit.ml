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

let of_packer_setting ~present (ps : packer_settings) =
  let open Stream.Table in
  let (uri :url) =
    { ip = ps.dst_ip
    ; port = ps.dst_port
    } in
  { url = Some uri
  ; present
  ; stream = ps.stream
  }

let find_and_rest (f : 'a -> bool) (l : 'a list) =
  let rec aux acc = function
    | [] -> None, List.rev acc
    | hd :: tl ->
       if f hd
       then Some hd, (List.rev acc) @ tl
       else aux (hd :: acc) tl
  in
  aux [] l

let make_streams (events : Board_protocol.events) =
  React.S.l2 ~eq:(Equal.list Stream.Table.equal_stream)
    (fun incoming config ->
      let merged, rest =
        List.fold_left (fun (acc, rest) (s : Stream.t) ->
            let opt, rest =
              find_and_rest (fun (c : packer_settings) ->
                  Stream.equal s c.stream) rest in
            let res = match opt with
              | Some (ps : packer_settings) ->
                 of_packer_setting ~present:true ps
              | None ->
                 { url = None
                 ; present = true
                 ; stream = s } in
            (res :: acc), rest)
          ([], config.packers) incoming in
      let rest = List.map (of_packer_setting ~present:false) rest in
      merged @ rest)
    events.in_streams events.config

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
  let state =
    React.S.l2 ~eq:Stream.Table.equal_source_state
      (fun s d ->
        match s, d with
        | `Fine, Some devi ->
           begin match devi.packers_num with
           | Some x when x > 0 -> `Limited x
           | Some _ | None -> `Forbidden
           end
        | _ -> `Forbidden)
      events.state events.devinfo in
  let (range : (Stream.Table.url * Stream.Table.url) list) =
    [ { ip = Ipaddr.V4.make 224 1 2 2; port = 1234 },
      { ip = Ipaddr.V4.make 239 255 255 255; port = 65535 }
    ] in
  let constraints = Board.{ state; range } in
  let set streams =
    match React.S.value events.state with
    | `Fine ->
       (try
          List.map (fun ({ url; stream } : Stream.Table.setting) ->
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
  ; log_source = (fun _ -> React.E.never) (* TODO implement source *)
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
