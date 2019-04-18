open Board_niitv_ts2ip_types
open Application_types

type notifs =
  { state : Topology.state React.signal
  ; status : status React.event
  ; devinfo : devinfo option React.signal
  ; config : config React.signal
  ; in_streams : Stream.t list React.signal
  ; out_streams : Stream.t list React.signal
  }

type api =
  { notifs : notifs
  ; kv : config Kv_v.rw
  ; channel : 'a. 'a Request.t -> ('a, Request.error) Lwt_result.t
  ; loop : unit -> unit Lwt.t
  ; push_data : Cstruct.t -> unit
  }

let msg_queue_size = 20

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.Infix.( >>= )

(* module Streams_setup : sig
 *   val full : Topology.topo_board ->
 *     devinfo option ->
 *     stream_settings list ->
 *     (packer_settings list,packers_error) result
 *   val simple : Topology.topo_board ->
 *     devinfo option ->
 *     Stream.t list ->
 *     (packer_settings list,packers_error) result
 * end = struct
 * 
 *   let to_packer_settings (b : Topology.topo_board)
 *       (s : stream_settings) : packer_settings option =
 *     match s.stream.orig_id, Stream.to_topo_port b s.stream with
 *     | TS_multi _, Some p ->
 *       Some { dst_ip = s.dst_ip
 *            ; dst_port = s.dst_port
 *            ; self_port = 2027
 *            ; enabled = s.enabled
 *            ; stream = s.stream
 *            ; socket = p.port }
 *     | _ -> None
 * 
 *   let full b devinfo streams =
 *     match Option.(devinfo >>= (fun x -> x.packers_num)) with
 *     | None -> Error `Undefined_limit
 *     | Some n ->
 *       let streams =
 *         List.filter (fun (s : stream_settings) ->
 *             match s.stream.orig_id with
 *             | TS_raw | TSoIP _ -> false
 *             | TS_multi _ -> true) streams in
 *       let len = List.length streams in
 *       if len > n then Error (`Limit_exceeded (n, len)) else
 *         let rec pack acc = function
 *           | [] -> acc
 *           | h :: tl ->
 *             begin match to_packer_settings b h with
 *               | Some pkr -> pack (pkr :: acc) tl
 *               | None -> pack acc tl
 *             end
 *         in
 *         Ok (pack [] streams)
 * 
 *   let succ_mcast addr =
 *     Int32.add (Ipaddr.V4.to_int32 addr) 1l
 *     |> Ipaddr.V4.of_int32
 * 
 *   let simple b devinfo streams =
 *     let settings =
 *       let rec pack dst_ip dst_port acc = function
 *         | [ ] -> acc
 *         | stream :: tl ->
 *           let s = { dst_ip; dst_port; enabled = true; stream } in
 *           pack (succ_mcast dst_ip) (succ dst_port) (s :: acc) tl
 *       in pack (Ipaddr.V4.make 224 1 2 2) 1234 [] streams
 *     in full b devinfo settings
 * 
 * end *)

(* let find_stream (b : Topology.topo_board)
 *     (stream : Stream.t)
 *     (port : int)
 *     (streams : Stream.t list) =
 *   List.find_opt (fun (t : Stream.t) ->
 *       let p = Stream.to_topo_port b t in
 *       match p with
 *       | Some p when Stream.equal t stream && p.port = port -> true
 *       | _ -> false) streams
 * 
 * let to_out_streams_s (b : Topology.topo_board)
 *     (status : status React.event)
 *     (streams : Stream.t list React.signal) =
 *   let status =
 *     let eq =
 *       Equal.pair equal_packer_settings equal_packer_status
 *       |> Equal.list in
 *     React.E.map (fun x -> x.packers_status) status
 *     |> React.S.hold ~eq [] in
 *   React.S.l2 ~eq:(Equal.list Stream.equal) (fun status streams ->
 *       List.fold_left (fun acc ((packer : packer_settings),
 *                                { bitrate; enabled; has_data; _ }) ->
 *                        let s = find_stream b packer.stream packer.socket streams in
 *                        match s, bitrate, enabled, has_data with
 *                        | Some s, Some _, true, true ->
 *                          let (stream : Stream.t) =
 *                            { source = s.source
 *                            ; orig_id = TSoIP { addr = packer.dst_ip
 *                                              ; port = packer.dst_port }
 *                            ; id = s.id
 *                            ; typ = s.typ
 *                            }
 *                          in stream :: acc
 *                        | _ -> acc) [] status)
 *     status streams *)

let create (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    streams_conv
    (kv : config Kv_v.rw)
    (control : int) =
  let state, set_state =
    React.S.create ~eq:Topology.equal_state `No_response in
  let devinfo, set_devinfo =
    React.S.create ~eq:(Boards.Util.Option.equal equal_devinfo) None in
  let status, set_status = React.E.create () in
  (* let in_streams = streams in (\* FIXME add ASI streams? *\)
   * let out_streams = to_out_streams_s board status streams in *)
  let (notifs : notifs) =
    { state
    ; devinfo
    ; in_streams = React.S.const []
    ; out_streams = React.S.const []
    ; status
    ; config = kv#s
    } in
  let req_queue, push_req_queue = Lwt_stream.create_bounded msg_queue_size in
  let rsp_queue, push_rsp_queue = Lwt_stream.create () in
  let push_data =
    let acc = ref None in
    let push (buf : Cstruct.t) =
      let buf = match !acc with
        | None -> buf
        | Some acc -> Cstruct.append acc buf in
      let parsed, new_acc = Parser.deserialize src buf in
      acc := new_acc;
      List.iter (fun x -> push_rsp_queue @@ Some x) parsed in
    push in
  let channel = fun req -> assert false in (* FIXME *)
  let loop = Fsm.start src sender req_queue rsp_queue kv set_state in
  let api =
    { notifs
    ; loop
    ; push_data
    ; channel
    ; kv
    } in
  Lwt.return_ok api
