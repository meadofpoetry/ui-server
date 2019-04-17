open Board_niitv_ts2ip_types
open Application_types

type api =
  { set_nw_mode : nw_settings -> unit Lwt.t
  ; set_streams : Stream.t list -> (unit, packers_error) Lwt_result.t
  ; set_packers : stream_settings list -> (unit, packers_error) Lwt_result.t
  ; config : unit -> config
  ; devinfo : unit -> devinfo option
  ; in_streams : unit -> Stream.t list
  ; out_streams : unit -> Stream.t list
  ; status : unit -> status option
  }

type notifs =
  { state : Topology.state React.signal
  ; status : status React.event
  ; devinfo : devinfo option React.signal
  ; config : config React.signal
  ; in_streams : Stream.t list React.signal
  ; out_streams : Stream.t list React.signal
  }

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.Infix.( >>= )

exception Unexpected_init

let has_board_info rsps =
  (Option.is_some % List.find_map (is_response Get_board_info)) rsps

module Streams_setup : sig
  val full : Topology.topo_board ->
    devinfo option ->
    stream_settings list ->
    (packer_settings list,packers_error) result
  val simple : Topology.topo_board ->
    devinfo option ->
    Stream.t list ->
    (packer_settings list,packers_error) result
end = struct

  let to_packer_settings (b : Topology.topo_board)
      (s : stream_settings) : packer_settings option =
    match s.stream.orig_id, Stream.to_topo_port b s.stream with
    | TS_multi _, Some p ->
      Some { dst_ip = s.dst_ip
           ; dst_port = s.dst_port
           ; self_port = 2027
           ; enabled = s.enabled
           ; stream = s.stream
           ; socket = p.port }
    | _ -> None

  let full b devinfo streams =
    match Option.(devinfo >>= (fun x -> x.packers_num)) with
    | None -> Error `Undefined_limit
    | Some n ->
      let streams =
        List.filter (fun (s : stream_settings) ->
            match s.stream.orig_id with
            | TS_raw | TSoIP _ -> false
            | TS_multi _ -> true) streams in
      let len = List.length streams in
      if len > n then Error (`Limit_exceeded (n, len)) else
        let rec pack acc = function
          | [] -> acc
          | h :: tl ->
            begin match to_packer_settings b h with
              | Some pkr -> pack (pkr :: acc) tl
              | None -> pack acc tl
            end
        in
        Ok (pack [] streams)

  let succ_mcast addr =
    Int32.add (Ipaddr.V4.to_int32 addr) 1l
    |> Ipaddr.V4.of_int32

  let simple b devinfo streams =
    let settings =
      let rec pack dst_ip dst_port acc = function
        | [ ] -> acc
        | stream :: tl ->
          let s = { dst_ip; dst_port; enabled = true; stream } in
          pack (succ_mcast dst_ip) (succ dst_port) (s :: acc) tl
      in pack (Ipaddr.V4.make 224 1 2 2) 1234 [] streams
    in full b devinfo settings

end

let find_stream (b : Topology.topo_board)
    (stream : Stream.t)
    (port : int)
    (streams : Stream.t list) =
  List.find_opt (fun (t : Stream.t) ->
      let p = Stream.to_topo_port b t in
      match p with
      | Some p when Stream.equal t stream && p.port = port -> true
      | _ -> false) streams

let to_out_streams_s (b : Topology.topo_board)
    (status : status React.event)
    (streams : Stream.t list React.signal) =
  let status =
    let eq =
      Equal.pair equal_packer_settings equal_packer_status
      |> Equal.list in
    React.E.map (fun x -> x.packers_status) status
    |> React.S.hold ~eq [] in
  React.S.l2 ~eq:(Equal.list Stream.equal) (fun status streams ->
      List.fold_left (fun acc ((packer : packer_settings),
                               { bitrate; enabled; has_data; _ }) ->
                       let s = find_stream b packer.stream packer.socket streams in
                       match s, bitrate, enabled, has_data with
                       | Some s, Some _, true, true ->
                         let (stream : Stream.t) =
                           { source = s.source
                           ; orig_id = TSoIP { addr = packer.dst_ip
                                             ; port = packer.dst_port }
                           ; id = s.id
                           ; typ = s.typ
                           }
                         in stream :: acc
                       | _ -> acc) [] status)
    status streams

let create
    logs
    sender
    (storage : config storage)
    (step_duration : float)
    (streams : Stream.t list React.signal)
    (board : Topology.topo_board) =
  let (module Logs : Logs.LOG) = logs in
  let state, state_push =
    React.S.create ~eq:Topology.equal_state `No_response in
  let devinfo, devinfo_push =
    React.S.create ~eq:(Equal.option equal_devinfo) None in
  let config, config_push =
    React.S.create ~eq:equal_config storage#get in
  let status, status_push = React.E.create () in
  let s_status =
    React.S.hold ~eq:(Equal.option equal_status) None
    @@ React.E.map Option.return status in
  let in_streams = streams in (* FIXME add ASI streams? *)
  let out_streams = to_out_streams_s board status streams in
  let (events : notifs) =
    { state
    ; devinfo
    ; in_streams
    ; out_streams
    ; status
    ; config
    } in
  let msgs = ref (Await_queue.create []) in
  let imsgs = ref (Queue.create []) in
  let api =
    { set_nw_mode =
        (fun (x : nw_settings) ->
           Logs.info (fun m ->
               m "got nw settings request: %s"
               @@ show_nw_settings x);
           let ss = storage#get.packers in
           enqueue_instant state imsgs sender storage (Set_board_mode (x,ss))
           >>= (fun _ -> config_push storage#get; Lwt.return_unit))
    ; set_streams =
        (fun (x : Stream.t list) ->
           Logs.info (fun m ->
               m "got set streams request: %s"
               @@ String.concat ";\n"
               @@ List.map Stream.show x);
           match Streams_setup.simple board (React.S.value devinfo) x with
           | Ok x ->
             let nw = storage#get.nw_mode in
             enqueue_instant state imsgs sender storage (Set_board_mode (nw,x))
             >>= (fun _ -> config_push storage#get; Lwt.return_ok ())
           | Error e -> Lwt.return_error e)
    ; set_packers =
        (fun (x : stream_settings list) ->
           Logs.info (fun m ->
               m "got set packers request: %s"
               @@ String.concat ";\n"
               @@ List.map show_stream_settings x);
           match Streams_setup.full board (React.S.value devinfo) x with
           | Ok x ->
             let nw = storage#get.nw_mode in
             enqueue_instant state imsgs sender storage (Set_board_mode (nw,x))
             >>= (fun _ -> config_push storage#get; Lwt.return_ok ())
           | Error e -> Lwt.return_error e)
    ; devinfo = (fun () -> React.S.value devinfo)
    ; config = (fun () -> storage#get)
    ; in_streams = (fun () -> React.S.value in_streams)
    ; out_streams = (fun () -> React.S.value out_streams)
    ; status = (fun () -> React.S.value s_status)
    }
  in
  events,
  api,
  (step msgs imsgs sender storage step_duration push_events logs)
