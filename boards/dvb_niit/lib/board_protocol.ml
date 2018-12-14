open Containers
open Board_types
open Storage.Options
open Boards.Board
open Boards.Pools
open Common

type 'a timestamped = 'a Common.Time.timestamped

type events =
  { devinfo : Device.devinfo option React.signal
  ; state : Topology.state React.signal
  ; config : Device.config React.event
  ; mode : (int * Device.mode) React.event
  ; measures : (Stream.t * Measure.t Time.timestamped) React.event
  ; params : (Stream.t * Params.t Time.timestamped) React.event
  ; plps : (Stream.t * Plp_list.t Time.timestamped) React.event
  ; raw_streams : Stream.Raw.t list React.signal
  ; streams : Stream.t list React.signal
  ; available_streams : Stream.t list React.signal
  }

type push_events =
  { mode : int * Device.mode -> unit
  ; measure : Stream.Multi_TS_ID.t * Measure.t timestamped -> unit
  ; params : Stream.Multi_TS_ID.t * Params.t timestamped -> unit
  ; plp_list : Stream.Multi_TS_ID.t * Plp_list.t timestamped -> unit
  ; state : Topology.state -> unit
  ; devinfo : Device.devinfo option -> unit
  }

type api =
  { get_devinfo : unit -> (Device.devinfo option) Lwt.t
  ; reset : unit -> unit Lwt.t
  ; set_mode : int * Device.mode -> (int * Device.mode_rsp) Lwt.t
  ; get_config : ?ids:int list -> unit -> Device.config Lwt.t
  ; get_measures : unit -> (Stream.t * Measure.t timestamped) list
  ; get_params : unit -> (Stream.t * Params.t timestamped) list
  ; get_plp_list : unit -> (Stream.t * Plp_list.t timestamped) list
  }

(* Board protocol implementation *)

let timeout = 3. (* seconds *)

module Make(Logs : Logs.LOG)(Src : Board_parser.Src) = struct

  module Parser = Board_parser.Make(Logs)(Src)

  open Parser

  let detect_msgs (send_req : 'a request -> unit Lwt.t) step_duration =
    let req = Get_devinfo in
    [ { send = (fun () -> send_req req)
      ; pred = (is_response req)
      ; timeout = Boards.Timer.steps ~step_duration timeout
      ; exn = None
    } ]

  let init_requests (c : Device.config) =
    List.map (fun (id, x) -> Set_mode (id, x)) c

  let init_msgs (send_req : 'a request -> unit Lwt.t)
        (step_duration : float)
        (config : Device.config)
        (receivers : int list) =
    let config =
      List.filter (fun (id, _) ->
          List.mem ~eq:(=) id receivers) config in
    List.map (fun x ->
        { send = (fun () -> send_req x)
        ; pred = (is_response x)
        ; timeout = Boards.Timer.steps ~step_duration timeout
        ; exn = None })
      (init_requests config)

  type event_raw =
    [ `Measure of int * Cstruct.t
    | `Params  of int * Cstruct.t
    | `Plps    of int * Cstruct.t
    ]
  type event_msg = (event_raw,event) msg
  type pool      = (event_raw,event) Pool.t

  module type M = sig
    val duration : float
    val timeout  : int
    val send     : event event_request -> unit Lwt.t
  end

  module type Probes = sig
    type t
    type event_raw =
      [ `Measure of int * Cstruct.t
      | `Params of int * Cstruct.t
      | `Plps of int * Cstruct.t
      ]
    val handle_event : push_events -> event -> t -> t
    val wait : t -> t
    val send : t -> unit Lwt.t
    val step : t -> t
    val responsed : t -> event_raw list -> event option
    val update_pool : Device.config -> t -> t
    val empty : t -> bool
    val last : t -> bool
    val next : t -> t
    val make : Device.config -> int list -> t
  end

  module Make_probes(M : M) : Probes = struct

    open Boards

    let meas_period = Timer.steps ~step_duration:M.duration 1.
    let plp_list_period = Timer.steps ~step_duration:M.duration 5.
    let params_period = Timer.steps ~step_duration:M.duration 5.

    module States = struct

      type 'a state =
        { prev : 'a option
        ; timer : int
        }

      type t =
        { measures : Measure.t timestamped state
        ; plp_list : Plp_list.t timestamped state
        ; params : Params.t timestamped state
        }

      let empty : t =
        { measures = { timer = meas_period; prev = None }
        ; plp_list = { timer = plp_list_period; prev = None }
        ; params = { timer = params_period; prev = None }
        }

    end

    type id = int
    type event_raw =
      [ `Measure of int * Cstruct.t
      | `Params of int * Cstruct.t
      | `Plps of int * Cstruct.t
      ]
    type t  =
      { states : (id * States.t) list
      ; pool : (event_raw,event) Pool.t
      }

    module Timer = struct
      let reset_measures (t : States.t) : States.t =
        { t with measures = { t.measures with timer = meas_period }}
      let reset_plp_list (t : States.t) : States.t =
        { t with plp_list = { t.plp_list with timer = plp_list_period }}
      let reset_params (t : States.t) : States.t =
        { t with params = { t.params with timer = params_period }}
    end

    let make_measure_probe id : event_msg =
      let req = Get_measure id in
      { send = (fun () -> M.send req)
      ; pred = is_event req
      ; timeout = M.timeout
      ; exn = None
      }

    let make_plp_list_probe id : event_msg =
      let req = Get_plp_list id in
      { send = (fun () -> M.send req)
      ; pred = is_event req
      ; timeout = M.timeout
      ; exn = None
      }

    let make_params_probe id : event_msg =
      let req = Get_params id in
      { send = (fun () -> M.send req)
      ; pred = is_event req
      ; timeout = M.timeout
      ; exn = None
      }

    let wait (t : t) : t =
      let pred : 'a. 'a States.state -> 'a States.state = fun x ->
        if x.timer <= 0
        then { x with timer = 0 }
        else { x with timer = x.timer - 1 } in
      let states =
        List.map (fun (id, (x : States.t)) ->
            id, ({ measures = pred x.measures
                 ; plp_list = pred x.plp_list
                 ; params = pred x.params } : States.t)) t.states
      in
      { t with states }

    let handle_event (pe : push_events) (e : event) (t : t) : t =
      let states = match e with
        | Measures ((id, m) as rsp) ->
           let id = Stream.Multi_TS_ID.stream_id id in
           (* push event in any case *)
           pe.measure rsp;
           (* update previous field *)
           begin match List.Assoc.get ~eq:(=) id t.states with
           | None -> None
           | Some tmr ->
              let tmr = { tmr with measures = { tmr.measures with prev = Some m }} in
              let tmr = Timer.reset_measures tmr in
              Some (List.Assoc.set ~eq:(=) id tmr t.states)
           end
        | Params ((id, p) as rsp) ->
           let id = Stream.Multi_TS_ID.stream_id id in
           begin match List.Assoc.get ~eq:(=) id t.states with
           | None -> None
           | Some tmr ->
              (* push event only if it is different
                 from the previous one or it is first *)
              begin match tmr.params.prev with
              | None -> pe.params rsp
              | Some prev ->
                 let eq = Params.equal in
                 if not (eq prev.data p.data)
                 then pe.params rsp
              end;
              (* update previous field *)
              let tmr = { tmr with params = { tmr.params with prev = Some p }} in
              let tmr = Timer.reset_params tmr in
              Some (List.Assoc.set ~eq:(=) id tmr t.states)
           end
        | Plp_list ((id, l) as rsp) ->
           let id = Stream.Multi_TS_ID.stream_id id in
           begin match List.Assoc.get ~eq:(=) id t.states with
           | None -> None
           | Some tmr ->
              (* push event only if it is different
                 from the previous one or it is first *)
              begin match tmr.plp_list.prev with
              | None -> pe.plp_list rsp
              | Some prev ->
                 let eq = Plp_list.equal in
                 if not (eq prev.data l.data) then pe.plp_list rsp
              end;
              (* update previous field *)
              let tmr = { tmr with plp_list = { tmr.plp_list with prev = Some l }} in
              let tmr = Timer.reset_plp_list tmr in
              Some (List.Assoc.set ~eq:(=) id tmr t.states)
           end
      in match states with Some states -> { t with states } | None -> t

    let make_pool (config : Device.config)
          (states : (id * States.t) list) : pool =
      List.fold_left (fun acc (id, (tmr : States.t)) ->
          match List.Assoc.get ~eq:(=) id config with
          | None -> acc
          | Some mode ->
             let ( ^:: ) = List.cons_maybe in
             let is_t2 = Device.equal_standard mode.standard T2 in
             let meas =
               if tmr.measures.timer > 0 then None
               else Some (make_measure_probe id) in
             let params =
               if (tmr.params.timer > 0) || (not is_t2) then None
               else Some (make_params_probe id) in
             let plps =
               if (tmr.plp_list.timer > 0) || (not is_t2) then None
               else Some (make_plp_list_probe id)
             in
             meas ^:: params ^:: plps ^:: acc) [] states
      |> Pool.create

    let step t = { t with pool = Pool.step t.pool }

    let responsed t msgs = Pool.responsed t.pool msgs

    let empty t = Pool.empty t.pool

    let update_pool config t =
      let pool = make_pool config t.states in
      { t with pool }

    let last t = Pool.last t.pool

    let next t = { t with pool = Pool.next t.pool }

    let send t = Pool.send t.pool ()

    let make config (receivers : int list) : t =
      let states = List.map (fun id -> id, States.empty) receivers in
      let pool = make_pool config states in
      { states; pool }

  end

  let wakeup_timeout t = t.pred `Timeout |> ignore

  let update_config (id : int)
        (mode : Device.mode)
        (config : Device.config) : Device.config =
    List.Assoc.set ~eq:(=) id mode config

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    (match msg with
     | Get_devinfo ->
        Logs.debug (fun m -> m "requesting devinfo");
        to_devinfo_req false
     | Reset ->
        Logs.debug (fun m -> m "requesting reset");
        to_devinfo_req true
     | Set_src_id () ->
        Logs.debug (fun m -> m "requesting source id setup (%d)" Src.source_id);
        to_src_id_req ()
     | Set_mode (id, m) ->
        Logs.debug (fun m -> m "requesting mode setup (%d)" id);
        to_mode_req id m)
    |> sender

  let send_event (type a) sender (msg : a event_request) : unit Lwt.t =
    (match msg with
     | Get_measure id ->
        Logs.debug (fun m -> m "requesting measures (%d)" id);
        to_measure_req id
     | Get_params id ->
        Logs.debug (fun m -> m "requesting params (%d)" id);
        to_params_req id
     | Get_plp_list id ->
        Logs.debug (fun m -> m "requesting plp list (%d)" id);
        to_plp_list_req id)
    |> sender

  let send (type a) state msgs sender (pe : push_events)
        timeout (msg : a request) : a Lwt.t =
    match React.S.value state with
    | `Fine ->
       let t, w = Lwt.wait () in
       let pred = function
         | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
         | l ->
            let open Option in
            is_response msg l >|= fun r ->
            (match msg with
             | Set_mode _ -> pe.mode ((fst r), (snd r).mode)
             | _ -> ());
            Lwt.wakeup w r
       in
       let send = fun () -> send_msg sender msg in
       msgs := Queue.append !msgs { send; pred; timeout; exn = None };
       t
    | _ -> Lwt.fail (Failure "board is not responding")

  let initial_timeout = -1

  let step msgs sender (storage : Device.config storage)
        step_duration (pe:push_events) =

    let module Probes =
      Make_probes(struct
          let duration = step_duration
          let send     = send_event sender
          let timeout  = Boards.Timer.steps ~step_duration timeout
        end) in

    let deserialize acc recvd =
      let recvd = concat_acc acc recvd in
      Parser.deserialize recvd in

    let rec first_step () =
      Logs.info (fun m -> m "start of connection establishment...");
      Queue.iter !msgs wakeup_timeout;
      msgs := Queue.create [];
      pe.state `No_response;
      let pool = Pool.create (detect_msgs (send_msg sender) step_duration) in
      Pool.send pool () |> ignore;
      `Continue (step_detect pool None)

    and step_detect detect_pool acc recvd =
      try
        let _, responses, acc = deserialize acc recvd in
        match Pool.responsed detect_pool responses with
        | Some devinfo ->
           pe.state `Init;
           pe.devinfo (Some devinfo);
           Logs.info (fun m ->
               m "connection established, \
                  board initialization started...");
           let req = Set_src_id () in
           let msg =
             { send = (fun () -> (send_msg sender) req)
             ; pred = (is_response req)
             ; timeout = Boards.Timer.steps ~step_duration timeout
             ; exn = None
             } in
           let pool = Pool.create [msg] in
           Pool.send pool () |> ignore;
           `Continue (step_init_src_id devinfo pool None)
        | _ -> `Continue (step_detect (Pool.step detect_pool) acc)
      with Timeout ->
        Logs.warn (fun m ->
            m "connection is not established after %g seconds, \
               restarting..." timeout);
        first_step ()

    and step_init_src_id devinfo pool acc recvd =
      try
        let _, responses, acc = deserialize acc recvd in
        match Pool.responsed pool responses with
        | None ->
           `Continue (step_init_src_id devinfo (Pool.step pool) acc)
        | Some id when id = Src.source_id ->
           Logs.debug (fun m -> m "source id setup done! id = %d" id);
           step_start_init devinfo
        | Some id ->
           Logs.warn (fun m ->
               m "failed setup source id! board returned id = %d, \
                  expected %d" id Src.source_id);
           first_step ()
      with Timeout ->
        Logs.warn (fun m -> m "timeout while initializing source id, \
                               restarting...");
        first_step ()

    and step_start_init devinfo =
      let probes = Probes.make storage#get devinfo.receivers in
      match init_msgs (send_msg sender) step_duration
              storage#get devinfo.receivers with
      | [] ->
         Logs.debug (fun m -> m "nothing to initialize, skipping...");
         pe.state `Fine;
         `Continue (step_ok_tee probes None)
      | lst ->
         Logs.debug (fun m -> m "found %d receivers to be initialized..."
                              @@ List.length lst);
         let init_pool = Pool.create lst in
         Pool.send init_pool () |> ignore;
         `Continue (step_init devinfo probes init_pool None)

    and step_init devinfo probes init_pool acc recvd =
      try
        let _, responses, acc = deserialize acc recvd in
        match Pool.responsed init_pool responses with
        | None ->
           `Continue (step_init devinfo probes (Pool.step init_pool) acc)
        | Some (id, mode) ->
           Logs.debug (fun m ->
               let s = Device.show_mode_rsp mode in
               m "receiver #%d initialized! mode = %s" id s);
           begin match Pool.last init_pool with
           | true ->
              Logs.info (fun m -> m "initialization done!");
              pe.state `Fine;
              `Continue (step_ok_tee probes acc)
           | false ->
              let init_pool = Pool.next init_pool in
              Pool.send init_pool () |> ignore;
              `Continue (step_init devinfo probes init_pool acc)
           end
      with Timeout ->
        Logs.warn (fun m -> m "timeout while initilizing receivers, \
                               restarting...");
        first_step ()

    and step_ok_tee probes acc recvd =
      match Queue.empty !msgs with
      | true ->
         let probes = Probes.update_pool storage#get probes in
         step_ok_probes_send probes acc recvd
      | false -> step_ok_requests_send probes acc recvd

    and step_ok_probes_send probes acc _ =
      let probes = Probes.wait probes in
      if Probes.empty probes
      then `Continue (step_ok_tee probes acc)
      else (Probes.send probes |> ignore;
            `Continue (step_ok_probes_wait probes acc))

    and step_ok_probes_wait probes acc recvd =
      let probes = Probes.wait probes in
      let events, _, acc = deserialize acc recvd in
      try
        (match Probes.responsed probes events with
         | None    ->
            let probes = Probes.step probes in
            `Continue (step_ok_probes_wait probes acc)
         | Some ev ->
            Logs.debug (fun m -> m "got probe response: %s"
                                 @@ show_event ev);
            let probes = Probes.handle_event pe ev probes in
            if Probes.last probes
            then `Continue (step_ok_tee probes acc)
            else let probes = Probes.next probes in
                 `Continue (step_ok_probes_send probes acc))
      with Timeout ->
        Logs.warn (fun m -> m "timeout while waiting for probe response, \
                               restarting...");
        first_step ()

    and step_ok_requests_send probes acc _ =
      let probes = Probes.wait probes in
      if Queue.empty !msgs
      then `Continue (step_ok_tee probes acc)
      else (Queue.send !msgs () |> ignore;
            `Continue (step_ok_requests_wait probes acc))

    and step_ok_requests_wait probes acc recvd =
      let probes = Probes.wait probes in
      let _, responses, acc = deserialize acc recvd in
      try
        match Queue.responsed !msgs responses with
        | None    ->
           msgs := Queue.step !msgs;
           `Continue (step_ok_requests_wait probes acc)
        | Some () ->
           msgs := Queue.next !msgs;
           `Continue (step_ok_requests_send probes acc)
      with Timeout ->
        Logs.warn (fun m ->
            m "timeout while waiting for client request response, \
               restarting...");
        first_step ()

    in
    first_step ()

  let mode_to_stream (id, ({ standard; channel } : Device.mode)) : Stream.Raw.t =
    let open Stream in
    let (freq : int64) = Int64.of_int channel.freq in
    let (bw : float) = match channel.bw with
      | Bw8 -> 8. | Bw7 -> 7. | Bw6 -> 6. in
    let (info : Source.t) = match standard with
      | T2 -> DVB_T2 { freq; bw; plp = channel.plp }
      | T -> DVB_T { freq; bw }
      | C -> DVB_C { freq; bw} in
    let (id : Multi_TS_ID.t) =
      Multi_TS_ID.make
        ~source_id:Src.source_id
        ~stream_id:id in
    { source = { info; node = Port 0 }
    ; id = TS_multi id
    ; typ = TS
    }

  let to_streams_s (config : Device.config React.signal) =
    React.S.map ~eq:(Equal.list Stream.Raw.equal)
      (List.map mode_to_stream) config

  let map_measures storage e =
    React.E.map (fun (id, (m : Measure.t timestamped)) ->
        let id' = Stream.Multi_TS_ID.stream_id id in
        match m.data.freq,
              List.Assoc.get ~eq:(=) id' storage#get with
        | Some x, Some (mode : Device.mode) ->
           let freq = Some (mode.channel.freq - x) in
           let data = { m.data with freq } in
           id, ({ m with data } : Measure.t timestamped)
        | _ ->
           let data = { m.data with freq = None } in
           id, { m with data }) e

  let map_streams (streams : Stream.t list React.signal)
        (e : (Stream.Multi_TS_ID.t * 'a) React.event) =
    React.S.sample (fun ((id, x) : Stream.Multi_TS_ID.t * 'a)
                        (streams : Stream.t list) ->
        match Stream.find_by_multi_id id streams with
        | None -> None
        | Some s -> Some (s, x)) e streams
    |> React.E.fmap Fun.id

  let to_available_streams (e : (Stream.t * Measure.t timestamped) React.event) =
    let eq = Stream.equal in
    React.E.map (fun (s, (m : Measure.t timestamped)) ->
        match m.data.lock, m.data.bitrate with
        | true, Some x when x > 0 -> `Found s
        | _ -> `Lost s) e
    |> React.S.fold ~eq:(Equal.list Stream.equal) (fun acc -> function
           | `Found x -> List.add_nodup ~eq x acc
           | `Lost x -> List.remove ~eq x acc) []

  let create sender streams_conv
        (storage : Device.config storage) step_duration =
    let s_devinfo, devinfo_push =
      React.S.create ~eq:(Equal.option Device.equal_devinfo) None in
    let e_mode, mode_push = React.E.create () in
    let e_measures, measures_push = React.E.create () in
    let e_params, params_push = React.E.create () in
    let e_plp_list, plp_list_push = React.E.create () in
    let s_state, state_push =
      React.S.create ~eq:Topology.equal_state `No_response in
    let s_config =
      React.S.fold ~eq:Device.equal_config (fun acc (id, mode) ->
          let c = List.Assoc.set ~eq:(=) id mode acc in
          storage#store c;
          c) storage#get e_mode in
    let hold_e ~eq f e  =
      React.E.fold (fun acc x ->
          List.Assoc.set ~eq:Stream.equal (f x) x acc) [] e
      |> React.E.map (List.map snd)
      |> React.S.hold ~eq [] in
    let raw_streams = to_streams_s s_config in
    let streams = streams_conv raw_streams in
    let measures = map_streams streams (map_measures storage e_measures) in
    let params = map_streams streams e_params in
    let plps = map_streams streams e_plp_list in
    let available_streams = to_available_streams measures in
    let (events : events) =
      { mode = e_mode
      ; measures
      ; params
      ; plps
      ; devinfo = s_devinfo
      ; config = React.S.changes s_config
      ; state = s_state
      ; raw_streams
      ; streams
      ; available_streams
      } in
    let (push_events : push_events) =
      { mode = mode_push
      ; measure = measures_push
      ; params = params_push
      ; state = state_push
      ; plp_list = plp_list_push
      ; devinfo = devinfo_push
      } in
    let eq f =
      let ts = Time.equal_timestamped f in
      Equal.list @@ Equal.pair Stream.equal ts in
    let s_measures = hold_e ~eq:(eq Measure.equal) fst events.measures in
    let s_params = hold_e ~eq:(eq Params.equal) fst events.params in
    let s_plp_list = hold_e ~eq:(eq Plp_list.equal) fst events.plps in
    let msgs = ref (Queue.create []) in
    let steps = Boards.Timer.steps ~step_duration timeout in
    let send x = send s_state msgs sender push_events steps x in
    let (api : api) =
      { get_devinfo = (fun () -> Lwt.return @@ React.S.value s_devinfo)
      ; reset = (fun () ->
        Logs.info (fun m -> m "got reset request");
        send Reset)
      ; set_mode = (fun r  ->
        Logs.info (fun m ->
            m "got set mode request for receiver #%d: %s"
              (fst r) (Device.show_mode (snd r)));
        send (Set_mode r))
      ; get_config = (fun ?ids () ->
        let config = storage#get in
        match ids with
        | None | Some [] -> Lwt.return config
        | Some ids ->
           List.filter (fun (id, _) -> List.mem ~eq:(=) id ids) config
           |> Lwt.return)
      ; get_measures = (fun () -> React.S.value s_measures)
      ; get_params = (fun () -> React.S.value s_params)
      ; get_plp_list = (fun () -> React.S.value s_plp_list)
      }
    in
    events,
    api,
    (step msgs sender storage step_duration push_events)

end
