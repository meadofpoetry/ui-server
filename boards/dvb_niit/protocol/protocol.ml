open Board_dvb_types
open Boards
open Boards.Util
open Application_types
open Util_react
open Parser

let ( >>= ) = Lwt.bind

type notifs =
  { devinfo : Device.info option React.signal
  ; state : Topology.state React.signal
  ; config : Device.config React.signal
  ; mode : (int * Device.mode) React.event
  ; measures : (int * Measure.t ts) list React.event
  ; params : (int * Params.t ts) list React.event
  ; plps : (int * Plp_list.t ts) list React.event
  ; raw_streams : Stream.Raw.t list React.signal
  ; streams : Stream.t list React.signal
  }

type api =
  { source_id : int
  ; notifs : notifs
  ; channel : 'a. 'a Parser.request -> ('a, error) result Lwt.t
  ; loop : (Cstruct.t list -> 'c Board.cc Lwt.t as 'c) Board.cc
  }
and error =
  | Not_responding
  | Timeout

let error_to_string = function
  | Not_responding -> "Board is not responding"
  | Timeout -> "Request timed out"

let timeout = 3. (* seconds *)

let rec merge_assoc ~eq ~acc = function
  | [] -> acc
  | (id, x) :: tl ->
     let acc = List.Assoc.set ~eq id x acc in
     merge_assoc ~eq ~acc tl

let detect_msgs (send_req : 'a request -> unit Lwt.t) step_duration =
  let req = Get_devinfo in
  [ Pools.{ send = (fun () -> send_req req)
          ; pred = (is_response req)
          ; timeout = Boards.Timer.steps ~step_duration timeout
          ; exn = None }
  ]

let init_requests (c : Device.config) =
  List.map (fun (id, x) -> Set_mode (id, x)) c

let init_msgs (send_req : 'a request -> unit Lwt.t)
      (step_duration : float)
      (config : Device.config)
      (receivers : int list) =
  let config =
    List.filter (fun (id, _) -> List.mem ~eq:(=) id receivers) config in
  List.map (fun x ->
      Pools.{ send = (fun () -> send_req x)
            ; pred = (is_response x)
            ; timeout = Boards.Timer.steps ~step_duration timeout
            ; exn = None })
    (init_requests config)

let wakeup_timeout (t : _ Pools.msg) =
  t.pred `Timeout |> ignore

let update_config (id : int)
      (mode : Device.mode)
      (config : Device.config) : Device.config =
  List.Assoc.set ~eq:(=) id mode config

let send_msg (type a) (src : Logs.src)
      (sender : Cstruct.t -> unit Lwt.t)
      (msg : a request) : unit Lwt.t =
  (match msg with
   | Get_devinfo ->
      Logs.debug ~src (fun m -> m "requesting devinfo");
      Serializer.make_devinfo_get_req false
   | Reset ->
      Logs.debug ~src (fun m -> m "requesting reset");
      Serializer.make_devinfo_get_req true
   | Set_src_id id ->
      Logs.debug ~src (fun m -> m "requesting source id setup (%d)" id);
      Serializer.make_src_id_set_req id
   | Set_mode (id, m) ->
      Logs.debug ~src (fun m -> m "requesting mode setup (%d)" id);
      Serializer.make_mode_set_req id m)
  |> sender

let send_event (type a) (src : Logs.src)
      (sender : Cstruct.t -> unit Lwt.t)
      (msg : a event_request) : unit Lwt.t =
  (match msg with
   | Get_measure id ->
      Logs.debug ~src (fun m -> m "requesting measures (%d)" id);
      Serializer.make_measure_get_req id
   | Get_params id ->
      Logs.debug ~src (fun m -> m "requesting params (%d)" id);
      Serializer.make_params_get_req id
   | Get_plp_list id ->
      Logs.debug ~src (fun m -> m "requesting plp list (%d)" id);
      Serializer.make_plp_list_get_req id)
  |> sender

let send (type a) (src : Logs.src)
      (state : Topology.state React.signal)
      msgs
      (sender : Cstruct.t -> unit Lwt.t)
      (pe : Probes.push_events)
      (timeout : int)
      (msg : a request) : (a, error) result Lwt.t =
  match React.S.value state with
  | `Fine ->
     let t, w = Lwt.wait () in
     let pred = function
       | `Timeout -> Lwt.wakeup w (Error Timeout); None
       | l ->
          match is_response msg l with
          | None -> None
          | Some r ->
             (match msg with
              | Set_mode _ -> pe.mode ((fst r), (snd r).mode)
              | _ -> ());
             Some (Lwt.wakeup w (Ok r)) in
     let send = fun () -> send_msg src sender msg in
     msgs := Pools.Queue.append !msgs { send; pred; timeout; exn = None };
     t
  | _ -> Lwt.return_error Not_responding

let step (src : Logs.src)
      msgs
      (sender : Cstruct.t -> unit Lwt.t)
      (config : Device.config React.signal)
      (source_id : int)
      (step_duration : float)
      (pe : Probes.push_events) =

  let module Probes =
    Probes.Make(struct
        let duration = step_duration
        let send = send_event src sender
        let timeout = Boards.Timer.steps ~step_duration timeout
      end) in

  let deserialize acc recvd =
    let recvd = Board.concat_acc acc recvd in
    deserialize src recvd in

  let rec first_step () =
    Logs.info ~src (fun m -> m "start of connection establishment...");
    Pools.Queue.iter !msgs wakeup_timeout;
    msgs := Pools.Queue.create [];
    pe.state `No_response;
    let pool =
      Pools.Pool.create
      @@ detect_msgs (send_msg src sender) step_duration in
    Pools.Pool.send pool ()
    >>= fun () -> Lwt.return @@ `Continue (step_detect pool None)

  and step_detect detect_pool acc recvd =
    try
      let _, responses, acc = deserialize acc recvd in
      match Pools.Pool.responsed detect_pool responses with
      | Some devinfo ->
         pe.state `Init;
         pe.devinfo (Some devinfo);
         Logs.info ~src (fun m ->
             m "connection established, \
                board initialization started...");
         let req = Set_src_id source_id in
         let msg =
           Pools.{ send = (fun () -> (send_msg src sender) req)
                 ; pred = is_response req
                 ; timeout = Boards.Timer.steps ~step_duration timeout
                 ; exn = None } in
         let pool = Pools.Pool.create [msg] in
         Pools.Pool.send pool ()
         >>= fun () -> Lwt.return @@ `Continue (step_init_src_id devinfo pool None)
      | _ -> Lwt.return @@ `Continue (step_detect (Pools.Pool.step detect_pool) acc)
    with Pools.Timeout ->
      Logs.warn ~src (fun m ->
          m "connection is not established after %g seconds, \
             restarting..." timeout);
      first_step ()

  and step_init_src_id devinfo pool acc recvd =
    try
      let _, responses, acc = deserialize acc recvd in
      match Pools.Pool.responsed pool responses with
      | None ->
         Lwt.return @@ `Continue (step_init_src_id devinfo (Pools.Pool.step pool) acc)
      | Some id when id = source_id ->
         Logs.debug (fun m -> m "source id setup done! id = %d" id);
         step_start_init devinfo
      | Some id ->
         Logs.warn ~src (fun m ->
             m "failed setup source id! board returned id = %d, \
                expected %d" id source_id);
         first_step ()
    with Pools.Timeout ->
      Logs.warn ~src (fun m ->
          m "timeout while initializing source id, restarting...");
      first_step ()

  and step_start_init (devinfo : Device.info) =
    let standards =
      List.map (fun (id, (v : Device.mode)) -> id, v.standard)
      @@ React.S.value config in
    let probes = Probes.make standards devinfo.receivers in
    match init_msgs (send_msg src sender) step_duration
            (React.S.value config) devinfo.receivers with
    | [] ->
       Logs.debug ~src (fun m -> m "nothing to initialize, skipping...");
       pe.state `Fine;
       Lwt.return @@ `Continue (step_ok_tee probes None)
    | lst ->
       Logs.debug ~src (fun m ->
           m "found %d receivers to be initialized..."
           @@ List.length lst);
       let init_pool = Pools.Pool.create lst in
       Pools.Pool.send init_pool ()
       >>= fun () -> Lwt.return @@ `Continue (step_init devinfo probes init_pool None)

  and step_init devinfo probes init_pool acc recvd =
    try
      let _, responses, acc = deserialize acc recvd in
      match Pools.Pool.responsed init_pool responses with
      | None ->
         let pool = Pools.Pool.step init_pool in
         Lwt.return @@ `Continue (step_init devinfo probes pool acc)
      | Some (id, mode) ->
         Logs.debug ~src (fun m ->
             let s = Device.show_mode_rsp mode in
             m "receiver #%d initialized! mode = %s" id s);
         begin match Pools.Pool.last init_pool with
         | true ->
            Logs.info ~src (fun m -> m "initialization done!");
            pe.state `Fine;
            Lwt.return @@ `Continue (step_ok_tee probes acc)
         | false ->
            let init_pool = Pools.Pool.next init_pool in
            Pools.Pool.send init_pool ()
            >>= fun () -> Lwt.return @@ `Continue (step_init devinfo probes init_pool acc)
         end
    with Pools.Timeout ->
      Logs.warn ~src (fun m ->
          m "timeout while initilizing receivers, restarting...");
      first_step ()

  and step_ok_tee (probes : Probes.t) (acc : Cstruct.t option) recvd =
    match Pools.Queue.empty !msgs with
    | true ->
       let (standards : (int * Device.standard) list) =
         List.map (fun (id, (x : Device.mode)) -> id, x.standard)
         @@ React.S.value config in
       let probes = Probes.update_pool standards probes in
       step_ok_probes_send probes acc recvd
    | false -> step_ok_requests_send probes acc recvd

  and step_ok_probes_send probes acc _ =
    let probes = Probes.wait probes in
    if Probes.empty probes
    then Lwt.return @@ `Continue (step_ok_tee probes acc)
    else Probes.send probes
         >>= fun () -> Lwt.return @@ `Continue (step_ok_probes_wait probes acc)

  and step_ok_probes_wait probes acc recvd =
    let probes = Probes.wait probes in
    let events, _, acc = deserialize acc recvd in
    try
      begin match Probes.responsed probes events with
      | None ->
         let probes = Probes.step probes in
         Lwt.return @@ `Continue (step_ok_probes_wait probes acc)
      | Some ev ->
         Logs.debug ~src (fun m ->
             m "got probe response: %s" @@ show_event ev);
         let probes = Probes.cons_event probes ev in
         if Probes.last probes
         then let probes = Probes.handle_events pe probes in
              Lwt.return @@ `Continue (step_ok_tee probes acc)
         else let probes = Probes.next probes in
              Lwt.return @@ `Continue (step_ok_probes_send probes acc)
      end
    with Pools.Timeout ->
      Logs.warn ~src (fun m ->
          m "timeout while waiting for probe response, restarting...");
      first_step ()

  and step_ok_requests_send (probes : Probes.t) acc _ =
    let probes = Probes.wait probes in
    if Pools.Queue.empty !msgs
    then Lwt.return @@ `Continue (step_ok_tee probes acc)
    else Pools.Queue.send !msgs ()
         >>= fun () -> Lwt.return @@ `Continue (step_ok_requests_wait probes acc)

  and step_ok_requests_wait (probes : Probes.t) acc recvd =
    let probes = Probes.wait probes in
    let _, responses, acc = deserialize acc recvd in
    try
      match Pools.Queue.responsed !msgs responses with
      | None ->
         msgs := Pools.Queue.step !msgs;
         Lwt.return @@ `Continue (step_ok_requests_wait probes acc)
      | Some () ->
         msgs := Pools.Queue.next !msgs;
         Lwt.return @@ `Continue (step_ok_requests_send probes acc)
    with Pools.Timeout ->
      Logs.warn ~src (fun m ->
          m "timeout while waiting for client request response, \
             restarting...");
      first_step ()

  in
  `Continue (fun _ -> first_step ())

(** Converts tuner mode to raw stream. *)
let mode_to_stream (source_id : int)
      (id, ({ standard; channel } : Device.mode)) : Stream.Raw.t =
  let open Stream in
  let (freq : int64) = Int64.of_int channel.freq in
  let (bw : float) = match channel.bw with
    | Bw8 -> 8.
    | Bw7 -> 7.
    | Bw6 -> 6. in
  let (info : Source.t) = match standard with
    | C -> DVB_C { freq; bw}
    | T -> DVB_T { freq; bw }
    | T2 -> DVB_T2 { freq; bw; plp = channel.plp } in
  let id = Multi_TS_ID.make ~source_id ~stream_id:id in
  { source = { info; node = Port 0 }
  ; id = TS_multi id
  ; typ = TS
  }

(** Converts device config signal to raw stream list signal. *)
let to_streams_s (source_id : int)
      (config : Device.config React.signal)
    : Stream.Raw.t list React.signal =
  React.S.map ~eq:(List.equal Stream.Raw.equal)
    (List.map (mode_to_stream source_id)) config

(** Converts absolute measured channel frequency value
    to frequency offset in measurements event. *)
let map_measures (config : Device.config React.signal)
      (e : (int * Measure.t ts) list React.event)
    : (int * Measure.t ts) list React.event =
  React.S.sample (fun l config ->
      List.filter_map
        (fun (id, ({ data; timestamp } : Measure.t ts)) ->
          match List.assoc_opt id config with
          | None -> None
          | Some (mode : Device.mode) ->
             let freq = match data.freq with
               | None -> None
               | Some x -> Some (mode.channel.freq - x) in
             let (data : Measure.t ts) =
               { data = { data with freq }; timestamp } in
             Some (id, data)) l) e config
  |> React.E.fmap (function [] -> None | l -> Some l)

let create (src : Logs.src)
      (sender : Cstruct.t -> unit Lwt.t)
      streams_conv
      (source_id : int)
      (kv : Device.config Kv_v.rw)
      (step_duration : float)
      (control : int) =
  let s_devinfo, devinfo_push =
    React.S.create ~eq:(Option.equal Device.equal_info) None in
  let e_mode, mode_push = React.E.create () in
  let e_measures, measures_push = React.E.create () in
  let params, params_push = React.E.create () in
  let plps, plp_list_push = React.E.create () in
  let s_state, state_push = React.S.create ~eq:Topology.equal_state `No_response in
  let raw_streams = to_streams_s source_id kv#s in
  let streams = streams_conv raw_streams in
  let measures = map_measures kv#s e_measures in
  let (notifs : notifs) =
    { mode = e_mode
    ; measures
    ; params
    ; plps
    ; devinfo = s_devinfo
    ; config = kv#s
    ; state = s_state
    ; raw_streams
    ; streams
    } in
  let (push_events : Probes.push_events) =
    { mode = mode_push
    ; measure = measures_push
    ; params = params_push
    ; state = state_push
    ; plp_list = plp_list_push
    ; devinfo = devinfo_push
    } in
  let msgs = ref (Pools.Queue.create []) in
  let steps = Boards.Timer.steps ~step_duration timeout in
  let send x = send src s_state msgs sender push_events steps x in
  let loop = step src msgs sender kv#s source_id step_duration push_events in
  { source_id
  ; notifs
  ; loop
  ; channel = send
  }
