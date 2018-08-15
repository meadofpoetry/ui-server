open Containers
open Common.Topology
open Boards.Board
open Board_types
open Common

open Lwt.Infix

module Data = struct

  type t      = Board_types.config
  let default = Board_types.config_default
  let dump    = Board_types.config_to_string
  let restore = Board_types.config_of_string

end

module Config_storage = Storage.Options.Make (Data)

let log_prefix control = Printf.sprintf "(Board TS2IP: %d) " control

let get_ports_sync board streams =
  let open React in
  let s = S.map (List.filter_map (Stream.to_topo_port board)) streams in
  List.fold_left (fun acc p ->
      S.map (List.mem ~eq:equal_topo_port p) s
      |> fun x -> Ports.add p.port x acc) Ports.empty board.ports

let create (b:topo_board) (streams:Stream.t list React.signal) _
      send db base step =
  let log_prefix = log_prefix b.control in
  let storage =
    Config_storage.create base ["board"; (string_of_int b.control)] in
  let events, api, step =
    Board_protocol.SM.create send storage step streams b log_prefix in
  let handlers = Board_api.handlers b.control api events in
  let available =
    React.S.l2 (fun incoming outgoing ->
        List.map (fun x ->
            let open Stream in
            match List.find_opt (fun o ->
                      match o.source.node with
                      | Stream s -> Stream.equal x s
                      | _ -> false) outgoing with
            | Some o ->
               (match o.orig_id with
                | TSoIP uri ->
                   Some ({ ip = uri.addr; port = uri.port }:Url.t), x
                | _ -> None, x)
            | None   -> None, x) incoming)
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
        [ { ip = Ipaddr.V4.make 224 1 2 2;       port = 1234 },
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
