open Containers
open Common.Topology
open Boards.Board
open Board_types
open Common

module Data = struct

  type t      = Board_types.config
  let default = Board_types.config_default
  let dump    = Board_types.config_to_string
  let restore = Board_types.config_of_string

end

module Config_storage = Storage.Options.Make (Data)

let get_ports_sync board streams =
  let open React in
  let s = S.map (List.filter_map (Stream.to_topo_port board)) streams in
  List.fold_left (fun acc p ->
      S.map (List.mem ~eq:equal_topo_port p) s
      |> fun x -> Ports.add p.port x acc) Ports.empty board.ports

let create (b:topo_board) (streams:Stream.t list React.signal) _
      send _ base step =
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
  let available =
    React.S.l2 (fun incoming outgoing ->
        List.map (fun x ->
            let open Stream in
            let stream = List.find_opt (fun o -> ID.equal o.id x.id) outgoing in
            match stream with
            | Some o ->
               begin match o.orig_id with
               | TSoIP uri ->
                  let uri : Url.t = { ip = uri.addr; port = uri.port } in
                  Some uri, x
               | _ -> assert false (* unreachable *)
               end
            | None -> None, x) incoming)
      events.in_streams events.out_streams in
  let constraints =
    { state =
        React.S.l2 (fun s d ->
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
          List.map (fun ((url:url),stream) ->
              if List.fold_left (fun acc x ->
                     if not @@ Common.Url.in_range x url
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
      List.fold_left (fun acc (p:topo_port)->
          Ports.add p.port (React.S.const true) acc)
        Ports.empty b.ports
  ; stream_handler =
      Some (object
            method streams     = available
            method set x       = set x
            method constraints = constraints
          end)
  ; state = (state :> < finalize : unit -> unit >)
  }
