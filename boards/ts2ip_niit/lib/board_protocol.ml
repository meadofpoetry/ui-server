open Containers
open Board_types
open Lwt.Infix
open Storage.Options
open Api.Handler
open Boards.Board
open Boards.Pools
open Common
open Board_parser

type api =
  { set_nw_mode : nw_settings          -> unit Lwt.t
  ; set_streams : Stream.t list        -> (unit,packers_error) Lwt_result.t
  ; set_packers : stream_settings list -> (unit,packers_error) Lwt_result.t
  ; config      : unit -> config
  ; devinfo     : unit -> devinfo option
  ; in_streams  : unit -> Stream.t list
  ; out_streams : unit -> Stream.t list
  ; status      : unit -> status option
  }

type events =
  { state       : Topology.state React.signal
  ; status      : status React.event
  ; devinfo     : devinfo option React.signal
  ; config      : config React.event
  ; in_streams  : Stream.t list React.signal
  ; out_streams : Stream.t list React.signal
  }

type push_events =
  { status  : status -> unit
  ; state   : Topology.state -> unit
  ; devinfo : devinfo option -> unit
  }

(* Board protocol implementation *)

let timeout = 5 (* seconds *)

module Timer = Boards.Timer

module SM = struct

  exception Unexpected_init

  let has_board_info rsps = Fun.(Option.is_some % List.find_map (is_response Get_board_info)) rsps

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    (match msg with
     | Get_board_info -> Get_board_info.serialize ())
    |> sender

  let send_instant (type a) sender (msg:a instant_request) : unit Lwt.t =
    (match msg with
     | Set_factory_mode x    -> Set_factory_mode.serialize x
     | Set_board_mode (nw,p) -> Set_board_mode.serialize (nw,p))
    |> sender

  let enqueue_instant (type a) state msgs sender storage (msg:a instant_request) : unit Lwt.t =
    match React.S.value state with
    | `Fine ->
       let t,w = Lwt.wait () in
       let send = fun () -> (send_instant sender msg) >>= (fun x -> Lwt.return @@ Lwt.wakeup w x) in
       let pred = fun _  -> None in
       let conf = storage#get in
       let _    = match msg with
         | Set_board_mode (nw,packers) -> storage#store ({ conf with nw_mode = nw; packers }:config)
         | _                           -> ()
       in
       msgs := Queue.append !msgs { send; pred; timeout = 0; exn = None };
       t
    | _ -> Lwt.fail (Failure "board is not responding")

  let step msgs imsgs sender (storage:config storage) step_duration (pe:push_events) log_prefix =

    let board_info_err_msg = "board info was received during normal operation, restarting..." in
    let no_status_msg t    = Printf.sprintf "no status received for %d seconds, restarting..."
                               (Timer.period t) in

    let module Parser = Board_parser.Make(struct let log_prefix = log_prefix end) in

    let fmt fmt = let fs = "%s" ^^ fmt in Printf.sprintf fs log_prefix in

    let wakeup_timeout (_,t) = t.pred `Timeout |> ignore in
    let events_push _ = function
      | `Status (brd,General x) ->
         let sms    = storage#get.packers in
         let pkrs   = List.take (List.length sms) x in
         let status = { board_status   = brd
                      ; packers_status = List.map2 Pair.make sms pkrs
                      }
         in
         Logs.debug (fun m -> m "%s" @@ fmt "got status event: %s" @@ show_status status);
         pe.status status
      | _ -> ()
    in

    let rec first_step () =
      Logs.info (fun m -> m "%s" @@ fmt "start of connection establishment...");
      Await_queue.iter !msgs wakeup_timeout;
      msgs  := Await_queue.create [];
      imsgs := Queue.create [];
      pe.state `No_response;
      send_msg sender Get_board_info |> Lwt.ignore_result;
      `Continue (step_detect (Timer.create ~step_duration timeout) None)

    and step_detect (timer:Timer.t) acc recvd =
      try
        let _,rsps,acc = Parser.deserialize (concat_acc acc recvd) in
        match List.find_map (is_response Get_board_info) rsps with
        | Some r ->
           pe.state `Init;
           pe.devinfo (Some r);
           let config = storage#get in
           send_instant sender (Set_factory_mode config.factory_mode) |> Lwt.ignore_result;
           send_instant sender (Set_board_mode (config.nw_mode,config.packers)) |> Lwt.ignore_result;
           Logs.info (fun m -> m "%s" @@ fmt "connection established, waiting for 'status' message");
           `Continue (step_ok_idle true r (Timer.reset timer) None)
        | None -> `Continue (step_detect (Timer.step timer) acc)
      with Timer.Timeout t ->
        (Logs.warn (fun m ->
             let s = fmt "connection is not established after %d seconds, restarting..." (Timer.period t) in
             m "%s" s);
         first_step ())

    and step_ok_idle is_init info (timer:Timer.t) acc recvd =
      try
        let events,rsps,acc = Parser.deserialize (concat_acc acc recvd) in
        if has_board_info rsps then raise_notrace Unexpected_init;
        Queue.send !imsgs () |> Lwt.ignore_result;
        imsgs := Queue.next !imsgs;
        match events with
        | [] -> `Continue (step_ok_idle is_init info (Timer.step timer) acc)
        | l  -> if is_init
                then (Logs.info (fun m -> m "%s" @@ fmt "initialization done!"); pe.state `Fine);
                List.iter (events_push info) l;
                `Continue (step_ok_idle false info (Timer.reset timer) acc)
      with
      | Unexpected_init -> Logs.warn (fun m -> m "%s" @@ fmt "%s" board_info_err_msg); first_step ()
      | Timer.Timeout t -> Logs.warn (fun m -> m "%s" @@ fmt "%s" @@ no_status_msg t); first_step ()

    in first_step ()

  module Streams_setup : sig
    val full   : Topology.topo_board ->
                 devinfo option ->
                 stream_settings list -> (packer_settings list,packers_error) result
    val simple : Topology.topo_board ->
                 devinfo option ->
                 Stream.t list -> (packer_settings list,packers_error) result
  end = struct

    let to_packer_settings (b:Topology.topo_board)
          (s:stream_settings) : packer_settings option =
      match s.stream.orig_id, Stream.to_topo_port b s.stream with
      | TS_multi id, Some p ->
         Some { dst_ip    = s.dst_ip
              ; dst_port  = s.dst_port
              ; self_port = 2027
              ; enabled   = s.enabled
              ; stream    = id
              ; socket    = p.port }
      | _ -> None

    let full b devinfo streams =
      match Option.(devinfo >>= (fun x -> x.packers_num)) with
      | None   -> Error `Undefined_limit
      | Some n ->
         let streams =
           List.filter (fun (s:stream_settings) ->
               match s.stream.orig_id with
               | TS_multi _ -> true
               | _          -> false) streams in
         let len = List.length streams in
         if len > n then Error (`Limit_exceeded (n,len))
         else let rec pack acc = function
                | []    -> acc
                | h::tl -> match to_packer_settings b h with
                           | Some pkr -> pack (pkr :: acc) tl
                           | None     -> pack acc tl
              in
              Ok (pack [] streams)

    let simple b devinfo streams =
      let settings =
        let succ_mcast addr = Int32.add (Ipaddr.V4.to_int32 addr) 1l |> Ipaddr.V4.of_int32 in
        let rec pack dst_ip dst_port acc = function
          | []         -> acc
          | stream::tl -> let s = { dst_ip; dst_port; enabled = true; stream } in
                          pack (succ_mcast dst_ip) (succ dst_port) (s :: acc) tl
        in pack (Ipaddr.V4.make 224 1 2 2) 1234 [] streams
      in full b devinfo settings

  end

  let to_out_streams_s b
        (status:status React.event)
        (streams:Stream.t list React.signal) =
    let find_stream (b:Topology.topo_board) (id:Stream.Multi_TS_ID.t)
          (port:int) (streams:Stream.t list) =
      List.find_opt (fun (t:Stream.t) ->
          let p = Stream.to_topo_port b t in
          match t.orig_id, p with
          | TS_multi x, Some p when Stream.Multi_TS_ID.equal x id
                                    && p.port = port -> true
          | _ -> false) streams in
    React.S.l2 (fun status streams ->
        List.fold_left (fun acc (packer,{bitrate;enabled;has_data;_}) ->
            let s = find_stream b packer.stream packer.socket streams in
            match s,bitrate,enabled,has_data with
            | Some s, Some _, true, true ->
               let (stream:Common.Stream.t) =
                 { source  = s.source
                 ; orig_id = TSoIP { addr = packer.dst_ip; port = packer.dst_port }
                 ; id      = s.id
                 ; typ     = s.typ
                 }
               in stream :: acc
            | _ -> acc) [] status)
      (React.S.hold [] (React.E.map (fun x -> x.packers_status) status)) streams

  let create sender
        (storage       : config storage)
        (step_duration : float)
        (streams       : Stream.t list React.signal)
        (board         : Topology.topo_board)
        (log_prefix    : string) =
    let state,state_push     = React.S.create `No_response in
    let devinfo,devinfo_push = React.S.create None in
    let config,config_push   = React.S.create storage#get in
    let status,status_push   = React.E.create () in

    let s_status    = React.S.hold None @@ React.E.map Option.return status in
    let in_streams  = streams in (* FIXME add ASI streams? *)
    let out_streams = to_out_streams_s board status streams in
    let (events:events) =
      { state
      ; devinfo
      ; in_streams
      ; out_streams
      ; status      = React.E.changes status
      ; config      = React.S.changes config
      }
    in
    let push_events =
      { status  = status_push
      ; state   = state_push
      ; devinfo = devinfo_push
      }
    in
    let msgs  = ref (Await_queue.create []) in
    let imsgs = ref (Queue.create []) in
    let api =
      { set_nw_mode = (fun (x:nw_settings) ->
          let ss = storage#get.packers in
          enqueue_instant state imsgs sender storage (Set_board_mode (x,ss))
          >>= (fun _ -> config_push storage#get; Lwt.return_unit))

      ; set_streams = (fun (x:Stream.t list) ->
        match Streams_setup.simple board (React.S.value devinfo) x with
        | Ok x    -> let nw = storage#get.nw_mode in
                     enqueue_instant state imsgs sender storage (Set_board_mode (nw,x))
                     >>= (fun _ -> config_push storage#get; Lwt.return_ok ())
        | Error e -> Lwt.return_error e)

      ; set_packers = (fun (x:stream_settings list) ->
        match Streams_setup.full board (React.S.value devinfo) x with
        | Ok x    -> let nw = storage#get.nw_mode in
                     enqueue_instant state imsgs sender storage (Set_board_mode (nw,x))
                     >>= (fun _ -> config_push storage#get; Lwt.return_ok ())
        | Error e -> Lwt.return_error e)

      ; devinfo     = (fun () -> React.S.value devinfo)
      ; config      = (fun () -> storage#get)
      ; in_streams  = (fun () -> React.S.value in_streams)
      ; out_streams = (fun () -> React.S.value out_streams)
      ; status      = (fun () -> React.S.value s_status)
      }
    in
    events,api,(step msgs imsgs sender storage step_duration push_events log_prefix)

end
