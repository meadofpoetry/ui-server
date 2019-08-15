open Application_types
open Board_niitv_ts2ip_types
open Board_niitv_ts2ip_protocol
open Netlib

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

let make_streams ~incoming_streams ~outgoing_streams config ports =
  React.S.l3 (fun incoming outgoing config ->
      let outgoing = List.filter_map (fun (mode : udp_mode) ->
          let url =
            Uri.with_host_v4 Uri.empty mode.dst_ip
            |> (fun uri -> Uri.with_scheme uri @@ Some "udp")
            |> (fun uri -> Uri.with_port uri @@ Some mode.dst_port)
            |> fun x -> Some x in
          let present, stream = match mode.stream with
            | ID id ->
              let stream =
                List.find_opt (fun (s : Stream.t) ->
                    match stream_to_socket ports s with
                    | None -> false
                    | Some socket ->
                      equal_socket socket mode.socket
                      && Stream.equal_container_id s.orig_id (TS_multi id))
                  outgoing in
              (match stream with
               | None -> false, None
               | Some _ as s -> true, s)
            | Full s ->
              let present = match Stream.find_by_id s.id outgoing with
                | None -> false
                | Some _ -> true in
              present, Some s in
          match stream with
          | None -> None
          | Some stream -> Some { Stream.Table. url; present; stream })
          config.mode.udp in
      let incoming = List.filter_map (fun (stream : Stream.t) ->
          match List.find_opt (fun ({ stream = s; _ } : Stream.Table.stream) ->
              Stream.ID.equal stream.id s.id) outgoing with
          | None -> Some { Stream.Table. url = None; present = true; stream }
          | Some _ -> None) incoming in
      List.sort_uniq Stream.Table.compare_stream (incoming @ outgoing))
    incoming_streams outgoing_streams config

let is_ipaddr_in_range range (a : Ipaddr.V4.t) =
  match Ipaddr.V4.compare a (fst range) with
  | x when x < 0 -> false
  | _ ->
    match Ipaddr.V4.compare a (snd range) with
    | x when x > 0 -> false
    | _ -> true

let apply_streams (api : Protocol.api) range ports streams =
  let range = Ipaddr.V4.range_to_pair range in
  match React.S.value api.notifs.state with
  | `No_response | `Init | `Detect -> Lwt.return_error `Forbidden
  | `Fine ->
    let rec check_loop acc = function
      | [] -> Ok (List.rev acc)
      | { Stream.Table. url; stream } :: tl ->
        match Netlib.Uri.host_v4 url,
              Uri.port url,
              stream_to_socket ports stream with
        | Some ip, Some port, Some socket ->
          (match stream.orig_id, is_ipaddr_in_range range ip with
           | (TS_raw | TS_multi _), true ->
             check_loop ((ip, port, stream, socket) :: acc) tl
           | _, false -> Error `Not_in_range
           | _ -> Error (`Internal_error "Invalid stream container ID"))
        | None, _, _ -> Error (`Internal_error "Invalid host in URI")
        | _, None, _ -> Error (`Internal_error "No port provided in URI")
        | _, _, None -> Error (`Internal_error "Invalid stream") in
    match check_loop [] streams with
    | Error _ as e -> Lwt.return e
    | Ok x ->
      let mode =
        List.map (fun (ip, port, stream, socket) ->
            print_endline @@ Ipaddr.V4.to_string ip;
            { dst_ip = ip
            ; dst_port = port
            ; enabled = true
            ; stream = Full stream
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
    (_ : Db.t)
    (kv : Kv.RW.t) : (Boards.Board.t, [> Boards.Board.error]) Lwt_result.t =
  Lwt.return @@ Boards.Board.create_log_src b
  >>=? fun (src : Logs.src) ->
  let default = Board_settings.default in
  Config.create ~default kv ["board"; (string_of_int b.control)]
  >>=? fun (cfg : config Kv_v.rw) ->
  Protocol.create src send streams (convert_streams b) cfg b.ports
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
  let range =
    (Ipaddr.V4.make 224 0 0 0, Ipaddr.V4.make 239 255 255 255)
    |> Ipaddr.V4.range_of_pair
    |> Option.get in
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
          method streams =
            make_streams
              ~outgoing_streams:api.notifs.outgoing_streams
              ~incoming_streams:api.notifs.incoming_streams
              cfg#s
              b.ports
          method set x = apply_streams api range b.ports x
          method constraints = constraints
        end)
    ; state = (state :> < finalize : unit -> unit Lwt.t >)
    ; gui_tabs = []
    } in
  Lwt.return_ok board
