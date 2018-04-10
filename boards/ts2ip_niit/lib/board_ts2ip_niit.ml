open Containers
open Common.Topology
open Api.Interaction
open Meta_board
open Board_types

module Api_handler = Api.Handler.Make (Common.User)

open Lwt.Infix

module Data = struct

  type t      = Board_types.config
  let default = Board_types.config_default
  let dump    = Board_types.config_to_string
  let restore = Board_types.config_of_string

end

module Config_storage = Storage.Options.Make (Data)

type 'a request = 'a Board_protocol.request

let create_sm = Board_protocol.SM.create

let stream_to_packer (b:topo_board) (s:stream_setting) : packer_setting option =
  match s.stream.id, Common.Stream.t_to_topo_port b s.stream with
  | `Ts id, Some p -> Lwt_io.printlf "Port: %d, sid: %04lx, dst ip: %s, dst port: %d, en:%s"
                                     p.port
                                     (Common.Stream.id_to_int32 id)
                                     (Ipaddr.V4.to_string s.dst_ip)
                                     s.dst_port
                                     (string_of_bool s.enabled)
                      |> ignore;
                      Some { base      = { stream   = s.stream
                                         ; dst_ip   = s.dst_ip
                                         ; dst_port = s.dst_port
                                         ; enabled  = s.enabled
                                         }
                           ; stream_id = Common.Stream.id_to_int32 id
                           ; port      = p.port
                           ; self_port = 2027
                           }
  | _ -> None

let create (b:topo_board) (streams:Common.Stream.t list React.signal) _ send db base step =
  let storage          = Config_storage.create base ["board"; (string_of_int b.control)] in
  let s_state, spush   = React.S.create `No_response in
  let convert          = stream_to_packer b in
  let events,api,devinfo,step = create_sm send storage spush step convert in
  let handlers         = Board_api.handlers b.control api events s_state streams in
  let s_sms            =
    React.E.map (fun status ->
        List.fold_left (fun acc ({stream;dst_ip;dst_port;_},status) ->
            match status.bitrate,status.enabled, status.has_data with
            | Some _, true, true -> let (stream : Common.Stream.t) = { source = Parent stream
                                                                     ; id = `Ip { ip = dst_ip; port = dst_port }
                                                                     ; description = stream.description }
                                    in stream :: acc
            | _ -> acc) [] status.packers_status)
                events.status
    |> React.S.hold []
  in
  let available =
    React.S.l2 (fun incoming outgoing ->
        List.map (fun x -> let open Common.Stream in
                           match List.find_opt (fun o -> match o.source with
                                                         | Parent s -> Common.Stream.equal x s
                                                         | _        -> false) outgoing with
                           | Some o -> (match o.id with
                                        | `Ip uri -> Some uri, x
                                        | _       -> None, x)
                           | None   -> None,x) incoming) streams s_sms
  in
  let set_state = React.S.l2 (fun s d -> match s,d with
                                         | `Fine,Some devi -> (match devi.packers_num with
                                                               | Some x when x > 0 -> `Limited x
                                                               | _                 -> `Forbidden)
                                         | _               -> `Forbidden)
                             s_state devinfo
  in
  let constraints = { state = set_state
                    ; range = [ ({ip=Ipaddr.V4.make 224 1 2 2; port = 1234},
                                 {ip=Ipaddr.V4.make 239 255 255 255; port = 65535})
                              ]
                    }
  in
  let set streams =
    match React.S.value s_state with
    | `Fine -> (try
                  List.map (fun ((url:Meta_board.url),stream) ->
                      if List.fold_left (fun acc x -> if not @@ Common.Uri.in_range x url
                                                      then false else acc) true constraints.range
                      then { stream;dst_ip=url.ip;dst_port=url.port;enabled=true }
                      else failwith "not in range") streams
                  |> api.set_streams_full
                  |> Lwt_result.map_err (function `Limit_exceeded x -> `Limit_exceeded x
                                                | `Undefined_limit  -> `Forbidden)
                with _ -> Lwt_result.fail `Not_in_range)
    | _ -> Lwt_result.fail `Forbidden
  in
  let state        = (object method finalize () = () end) in
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = s_sms
  ; step           = step
  ; connection     = s_state
  ; ports_active   = List.fold_left (fun acc (p:topo_port)-> Ports.add p.port (React.S.const true) acc)
                                    Ports.empty b.ports
  ; settings_page  = ("TS2IP", React.S.const (Tyxml.Html.div []))
  ; widgets_page   = [("TS2IP", React.S.const (Tyxml.Html.div []))]
  ; stream_handler = Some (object
                             method streams     = available
                             method set x       = set x
                             method constraints = constraints
                           end)
  ; state          = (state :> < finalize : unit -> unit >)
  }
