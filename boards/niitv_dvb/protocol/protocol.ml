open Board_niitv_dvb_types
open Boards
open Boards.Util
open Application_types
open Util_react

let ( >>= ) = Lwt.bind

type notifs =
  { devinfo : Device.info option React.signal
  ; state : Topology.state React.signal
  ; config : Device.config React.signal
  ; measures : (int * Measure.t ts) list React.event
  ; params : (int * Params.t ts) list React.event
  ; plps : (int * Plp_list.t ts) list React.event
  ; raw_streams : Stream.Raw.t list React.signal
  ; streams : Stream.t list React.signal
  }

type api =
  { source_id : int
  ; kv : Device.config Kv_v.rw
  ; notifs : notifs
  ; channel : 'a. 'a Parser.request -> ('a, error) Lwt_result.t
  ; loop : (Cstruct.t list -> 'c Board.cc Lwt.t as 'c) Board.cc
  ; model : Model.t
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

let detect_msgs (send : 'a Parser.request -> unit Lwt.t) =
  let req = Parser.Get_devinfo in
  [ Pools.make_msg ~send:(fun () -> send req)
      ~timeout:(fun () -> Lwt_unix.sleep timeout)
      ~resolve:(Parser.is_response req)
      ()
  ]

let init_msgs (send : 'a Parser.request -> unit Lwt.t)
      (config : Device.config)
      (receivers : int list) =
  let config = List.filter (fun (id, _) -> List.mem ~eq:(=) id receivers) config in
  List.map (fun req ->
      Pools.make_msg ~send:(fun () -> send req)
        ~timeout:(fun () -> Lwt_unix.sleep timeout)
        ~resolve:(Parser.is_response req)
        ())
  @@ List.map (fun x -> Parser.Set_mode x) config

let update_config (id : int)
      (mode : Device.mode)
      (config : Device.config) : Device.config =
  List.Assoc.set ~eq:(=) id mode config

let send_msg (type a) (src : Logs.src)
      (sender : Cstruct.t -> unit Lwt.t)
      (msg : a Parser.request) : unit Lwt.t =
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
      (msg : a Parser.event_request) : unit Lwt.t =
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
      (msgs : (_, unit) Pools.Queue.t ref)
      (kv : Device.config Kv_v.rw)
      (state : Topology.state React.signal)
      (sender : Cstruct.t -> unit Lwt.t)
      (timeout : float)
      (req : a Parser.request) =
  match React.S.value state with
  | `Init | `No_response -> Lwt.return_error Not_responding
  | `Fine ->
     (* FIXME looks ugly, do some refactoring later on. *)
     let t, w = Lwt.wait () in
     let msg =
       Pools.make_msg
         ~send:(fun () -> send_msg src sender req)
         ~timeout:(fun () -> Lwt_unix.sleep timeout)
         ~resolve:(fun msg ->
           match Parser.is_response req msg with
           | None -> None
           | Some x -> Lwt.wakeup_later w (Ok x); Some ())
         () in
     let t', msgs' = Pools.Queue.snoc !msgs msg in
     msgs := (msgs' :> (_, unit) Pools.Queue.t);
     t' >>= function
     | Ok () -> t
     | Error `Timeout -> Lwt.return_error Timeout
     | Error `Interrupted -> Lwt.return_error Not_responding

let step (src : Logs.src)
      msgs
      (sender : Cstruct.t -> unit Lwt.t)
      (config : Device.config React.signal)
      (source_id : int)
      (pe : Probes.push_events) =

  let deserialize acc recvd =
    let recvd = Board.concat_acc acc recvd in
    Parser.deserialize src recvd in

  let rec first_step () =
    Logs.info ~src (fun m -> m "start of connection establishment...");
    msgs := Pools.Queue.invalidate !msgs;
    pe.state `No_response;
    let pool = Pools.Pool.create @@ detect_msgs (send_msg src sender) in
    Pools.Pool.send pool
    >>= fun pool ->
    Lwt.return @@ `Continue (step_detect pool None)

  and step_detect pool acc recvd =
    let _, responses, acc = deserialize acc recvd in
    Pools.Pool.apply pool responses;
    Pools.Pool._match pool
      ~pending:(fun pool -> Lwt.return @@ `Continue (step_detect pool acc))
      ~resolved:(fun _ devinfo ->
        Logs.info ~src (fun m ->
            m "connection established, board initialization started...");
        pe.state `Init;
        pe.devinfo (Some devinfo);
        let req = Parser.Set_src_id source_id in
        let msg =
          Pools.make_msg
            ~send:(fun () -> send_msg src sender req)
            ~timeout:(fun () -> Lwt_unix.sleep timeout)
            ~resolve:(Parser.is_response req)
            () in
        Pools.Pool.(send (create [msg]))
        >>= fun pool ->
        Lwt.return @@ `Continue (step_init_src_id devinfo pool None))
      ~error:(fun _ ->
        function `Timeout ->
          Logs.warn ~src (fun m ->
              m "connection is not established after %g seconds, \
                 restarting..." timeout);
          first_step ())
      ~not_sent:(fun _ -> assert false)

  and step_init_src_id devinfo pool acc recvd =
    let _, responses, acc = deserialize acc recvd in
    Pools.Pool.apply pool responses;
    Pools.Pool._match pool
      ~pending:(fun pool -> Lwt.return @@ `Continue (step_init_src_id devinfo pool acc))
      ~resolved:(fun _ -> function
        | id when id = source_id ->
           Logs.debug ~src (fun m -> m "source id setup done! id = %d" id);
           step_start_init devinfo
        | id ->
           Logs.warn ~src (fun m ->
               m "failed setting up source id! board returned id = %d, \
                  but expected %d" id source_id);
           first_step ())
      ~error:(fun _ ->
        function `Timeout ->
          Logs.warn ~src (fun m ->
              m "timeout while initializing source id, restarting...");
          first_step ())
      ~not_sent:(fun _ -> assert false)

  and step_start_init (devinfo : Device.info) =
    let standards =
      List.map (fun (id, (v : Device.mode)) -> id, v.standard)
      @@ React.S.value config in
    let probes = Probes.make timeout (send_event src sender) standards devinfo.receivers in
    match init_msgs (send_msg src sender) (React.S.value config) devinfo.receivers with
    | [] ->
       Logs.debug ~src (fun m ->
           m "nothing to initialize, skipping...");
       pe.state `Fine;
       Lwt.return @@ `Continue (step_ok_tee probes None)
    | l ->
       Logs.debug ~src (fun m ->
           m "found %d receivers to be initialized..."  @@ List.length l);
       Pools.Pool.(send @@ create l)
       >>= fun pool ->
       Lwt.return @@ `Continue (step_init devinfo probes pool None)

  and step_init devinfo probes pool acc recvd =
    let _, responses, acc = deserialize acc recvd in
    Pools.Pool.apply pool responses;
    Pools.Pool._match pool
      ~pending:(fun pool -> Lwt.return @@ `Continue (step_init devinfo probes pool acc))
      ~resolved:(fun pool (id, mode) ->
        Logs.debug ~src (fun m ->
            let s = Device.show_mode_rsp mode in
            m "receiver #%d initialized! mode = %s" id s);
        if Pools.Pool.is_last pool
        then (
          Logs.info ~src (fun m -> m "initialization done!");
          pe.state `Fine;
          Lwt.return @@ `Continue (step_ok_tee probes acc))
        else (
          Pools.Pool.send pool
          >>= fun pool ->
          Lwt.return @@ `Continue (step_init devinfo probes pool acc)))
      ~error:(fun _ ->
        function `Timeout ->
          Logs.warn ~src (fun m ->
              m "timeout while initializing receivers, restarting...");
          first_step ())
      ~not_sent:(fun _ -> assert false)

  and step_ok_tee (probes : Probes.t) (acc : Cstruct.t option) recvd =
    match Pools.Queue.is_empty !msgs with
    | true ->
       let (standards : (int * Device.standard) list) =
         List.map (fun (id, (x : Device.mode)) -> id, x.standard)
         @@ React.S.value config in
       let probes = Probes.update standards probes in
       step_ok_probes_send probes acc recvd
    | false -> step_ok_requests_send probes acc recvd

  and step_ok_probes_send probes acc _ =
    if Probes.is_empty probes
    then Lwt.return @@ `Continue (step_ok_tee probes acc)
    else
      Probes.send probes
      >>= fun probes ->
      Lwt.return @@ `Continue (step_ok_probes_wait probes acc)

  and step_ok_probes_wait probes acc recvd =
    let events, _, acc = deserialize acc recvd in
    Probes.apply probes events;
    Probes._match probes
      ~pending:(fun probes ->
        Lwt.return @@ `Continue (step_ok_probes_wait probes acc))
      ~resolved:(fun probes ev ->
        Logs.debug ~src (fun m -> m "got probe response: %s" @@ Parser.show_event ev);
        Lwt.return
        @@ if Probes.is_last probes
           then `Continue (step_ok_tee (Probes.handle_events pe probes) acc)
           else `Continue (step_ok_probes_send probes acc))
      ~error:(fun _ ->
        function `Timeout ->
          Logs.warn ~src (fun m ->
              m "timeout while waiting for probe response, restarting...");
          first_step ())
      ~not_sent:(fun probes ->
        Lwt.return @@ `Continue (step_ok_tee probes acc))

  and step_ok_requests_send (probes : Probes.t) acc _ =
    if Pools.Queue.is_empty !msgs
    then Lwt.return @@ `Continue (step_ok_tee probes acc)
    else (
      Pools.Queue.send !msgs
      >>= fun pool ->
      msgs := pool;
      Lwt.return @@ `Continue (step_ok_requests_wait probes acc))

  and step_ok_requests_wait (probes : Probes.t) acc recvd =
    let _, responses, acc = deserialize acc recvd in
    Pools.Queue.apply !msgs responses;
    Pools.Queue._match !msgs
      ~pending:(fun pool ->
        msgs := pool;
        Lwt.return @@ `Continue (step_ok_requests_wait probes acc))
      ~resolved:(fun pool _ ->
        msgs := pool;
        Lwt.return @@ `Continue (step_ok_requests_send probes acc))
      ~not_sent:(fun pool ->
        msgs := pool;
        Lwt.return @@ `Continue (step_ok_requests_send probes acc))
      ~error:(fun pool -> function
        | `Interrupted ->
           msgs := pool;
           Lwt.return @@ `Continue (step_ok_requests_send probes acc)
        | `Timeout ->
           Logs.warn ~src (fun m ->
               m "timeout while waiting for client request response, restarting...");
           first_step ())
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
      (control : int)
      (db : Db.t) =
  let open Util_react in
  let ( >>= ) = Lwt_result.bind in
  let devinfo, set_devinfo = S.create ~eq:(Option.equal Device.equal_info) None in
  let measures, set_measures = E.create () in
  let params, set_params = E.create () in
  let plps, set_plps = E.create () in
  let state, set_state = S.create ~eq:Topology.equal_state `No_response in
  let raw_streams = to_streams_s source_id kv#s in
  let streams = streams_conv raw_streams in
  let (notifs : notifs) =
    { measures = map_measures kv#s measures
    ; params
    ; plps
    ; devinfo
    ; config = kv#s
    ; state
    ; raw_streams
    ; streams
    } in
  Model.create src control measures db
  >>= fun model ->
  let (push_events : Probes.push_events) =
    { measure = set_measures
    ; params = set_params
    ; state = set_state
    ; plp_list = set_plps
    ; devinfo = set_devinfo
    } in
  let msgs = ref (Pools.Queue.create []) in
  let loop = step src msgs sender kv#s source_id push_events in
  let send req = send src msgs kv state sender timeout req in
  let api =
    { source_id
    ; notifs
    ; loop
    ; channel = send
    ; model
    ; kv
    } in
  Lwt.return_ok api
