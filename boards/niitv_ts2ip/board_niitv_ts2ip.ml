open Application_types
open Board_niitv_ts2ip_types
open Board_niitv_ts2ip_protocol
open Util_react

let ( >>=? ) = Lwt_result.( >>= )

let ( >>= ) = Lwt.( >>= )

let ( % ) f g x = f (g x)

module Config = Kv_v.RW(Board_settings)

let get_ports_active ports =
  List.fold_left (fun acc { Topology. port; _ } ->
      Boards.Board.Ports.add port React.S.Bool.one acc)
    Boards.Board.Ports.empty ports

let get_ports_sync ports (status : device_status React.event) =
  List.fold_left (fun acc { Topology. port; _ } ->
      let s =
        React.S.hold ~eq:(=) false
        @@ React.E.map (fun ({ sync; _ } : device_status) ->
            List.exists ((=) port % socket_to_enum) sync) status in
      Boards.Board.Ports.add port s acc)
    Boards.Board.Ports.empty ports

let find_and_rest (f : 'a -> bool) (l : 'a list) =
  let rec aux acc = function
    | [] -> None, List.rev acc
    | hd :: tl ->
      if f hd
      then Some hd, (List.rev acc) @ tl
      else aux (hd :: acc) tl
  in
  aux [] l

(* let make_streams
 *     (input_streams : Stream.t list React.signal)
 *     (config : config React.signal) =
 *   React.S.l2 ~eq:(Util_equal.List.equal Stream.Table.equal_stream)
 *     (fun incoming config ->
 *        let merged, rest =
 *          List.fold_left (fun (acc, rest) (s : Stream.t) ->
 *              let opt, rest =
 *                find_and_rest (fun (c : packer_settings) ->
 *                    Stream.equal s c.stream) rest in
 *              let res = match opt with
 *                | Some (ps : packer_settings) ->
 *                  of_packer_setting ~present:true ps
 *                | None ->
 *                  { url = None
 *                  ; present = true
 *                  ; stream = s } in
 *              (res :: acc), rest)
 *            ([], config.mode.udp) incoming in
 *        let rest = List.map (of_packer_setting ~present:false) rest in
 *        merged @ rest)
 *     input_streams config *)

let is_ipaddr_in_range range (a : Ipaddr.V4.t) =
  match Ipaddr.V4.compare a (fst range) with
  | x when x < 0 -> false
  | _ ->
    match Ipaddr.V4.compare a (snd range) with
    | x when x > 0 -> false
    | _ -> true

let apply_streams (api : Protocol.api) range ports streams =
  match React.S.value api.notifs.state with
  | `No_response | `Init | `Detect -> Lwt.return_error `Forbidden
  | `Fine ->
    let rec check_loop acc = function
      | [] -> Ok (List.rev acc)
      | { Stream.Table. url; stream } :: tl ->
        match Uri.host url,
              Uri.port url,
              stream_to_socket ports stream with
        | Some host, Some port, Some socket ->
          let stream_id = match stream.orig_id with
            | TS_raw -> Some (Stream.Multi_TS_ID.of_int32_pure 0l)
            | TS_multi x -> Some x
            | _ -> None in
          (match stream_id, Ipaddr.V4.of_string host with
           | None, _ -> Error (`Internal_error "Invalid stream container ID")
           | _, Error `Msg s -> Error (`Internal_error s)
           | Some id, Ok ip ->
             if is_ipaddr_in_range range ip
             then check_loop ((ip, port, id, stream.id, socket) :: acc) tl
             else Error `Not_in_range)
        | None, _, _ -> Error (`Internal_error "No host provided in URI")
        | _, None, _ -> Error (`Internal_error "No port provided in URI")
        | _, _, None -> Error (`Internal_error "Invalid stream") in
    match check_loop [] streams with
    | Error _ as e -> Lwt.return e
    | Ok x ->
      let mode =
        List.map (fun (ip, port, stream, stream_id, socket) ->
            { dst_ip = ip
            ; dst_port = port
            ; enabled = true
            ; stream
            ; stream_id = Some stream_id
            ; self_port = 2027
            ; socket
            }) x in
      Api_transmitter.set_mode_ api mode
      >>= function
      | `Error e -> Lwt.return_error @@ `Internal_error e
      | `Unit -> Lwt.return_ok ()

let create (b : Topology.topo_board)
    (streams : Stream.t list React.signal)
    (convert_streams : Topology.topo_board ->
     Stream.Raw.t list React.signal ->
     Stream.t list React.signal)
    (send : Cstruct.t -> unit Lwt.t)
    (db : Db.t)
    (kv : Kv.RW.t) : (Boards.Board.t, [> Boards.Board.error]) Lwt_result.t =
  Lwt.return @@ Boards.Board.create_log_src b
  >>=? fun (src : Logs.src) ->
  let default = Board_settings.default in
  Config.create ~default kv ["board"; (string_of_int b.control)]
  >>=? fun (cfg : config Kv_v.rw) ->
  Protocol.create src send streams (convert_streams b) cfg b.ports b.control
  >>=? fun (api : Protocol.api) ->
  let state = object
    method finalize () = Lwt.return ()
  end in
  let source_state =
    React.S.l2 ~eq:Stream.Table.equal_source_state
      (fun s d ->
         match s, d with
         | `Fine, Some { packers_num; _ } -> `Limited packers_num
         | _ -> `Forbidden)
      api.notifs.state api.notifs.devinfo in
  let range = (Ipaddr.V4.make 224 1 2 2, Ipaddr.V4.make 239 255 255 255) in
  let constraints = { Boards.Board. state = source_state; range = [range] } in
  let board =
    { Boards.Board.
      http = Board_niitv_ts2ip_http.handlers b.control api
    ; ws = Board_niitv_ts2ip_http.ws b.control api
    ; templates = []
    ; control = b.control
    ; streams_signal = api.notifs.outgoing_streams
    ; log_source = (fun _ -> React.E.never) (* TODO implement source *)
    ; loop = api.loop
    ; push_data = api.push_data
    ; connection = api.notifs.state
    ; ports_sync = get_ports_sync b.ports api.notifs.device_status
    ; ports_active = get_ports_active b.ports
    ; stream_handler = Some (object
          method streams = assert false (* make_streams events *)
          method set x = apply_streams api range b.ports x
          method constraints = constraints
        end)
    ; state = (state :> < finalize : unit -> unit Lwt.t >)
    } in
  Lwt.return_ok board
