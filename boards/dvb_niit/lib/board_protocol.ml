open Containers
open Board_types
open Storage.Options
open Boards.Board
open Boards.Pools

include Board_parser

type events =
  { devinfo  : devinfo option React.signal
  ; state    : Common.Topology.state React.signal
  ; config   : config React.event
  ; mode     : (int * mode) React.event
  ; lock     : (int * lock) React.event
  ; measures : (int * measures) React.event
  ; params   : (int * params) React.event
  ; plp_list : (int * plp_list) React.event
  ; streams  : Common.Stream.Raw.t list React.signal
  }

type push_events =
  { mode     : int * mode            -> unit
  ; lock     : int * lock            -> unit
  ; measure  : int * measures        -> unit
  ; params   : int * params          -> unit
  ; plp_list : int * plp_list        -> unit
  ; state    : Common.Topology.state -> unit
  ; devinfo  : devinfo option        -> unit
  }

type api =
  { get_devinfo  : unit       -> (devinfo option) Lwt.t
  ; reset        : unit       -> unit Lwt.t
  ; set_mode     : int * mode -> (int * mode_rsp) Lwt.t
  ; get_config   : unit       -> config
  ; get_lock     : unit       -> (int * lock) list
  ; get_measures : unit       -> (int * measures) list
  ; get_params   : unit       -> (int * params) list
  ; get_plp_list : unit       -> (int * plp_list) list
  }

(* Board protocol implementation *)

let timeout = 3 (* seconds *)

let detect_msgs (send_req : 'a request -> unit Lwt.t) step_duration =
  let req = Get_devinfo in
  [ { send    = (fun () -> send_req req)
    ; pred    = (is_response req)
    ; timeout = Boards.Timer.steps ~step_duration timeout
    ; exn     = None
  } ]

let init_requests =
  let get_mode id x = match x.standard with
    | T2 -> id, { standard = T2; channel = x.t2 }
    | T  -> id, { standard = T ; channel = x.t  }
    | C  -> id, { standard = C ; channel = x.c  }
  in
  List.map (fun (id, x) -> Set_mode (get_mode id x))

let init_msgs (send_req : 'a request -> unit Lwt.t)
      (step_duration : float)
      (config        : config)
      (receivers     : int list) =
  (* If config for a receiver is empty, create default one *)
  let config =
    List.map (fun id ->
        match List.find_opt (fun (x,_) -> id = x) config with
        | Some x -> x
        | None   ->
           id, { standard = T2
               ; t2       = default ()
               ; t        = default ()
               ; c        = default () }) receivers in
  List.map (fun x ->
      { send    = (fun () -> send_req x)
      ; pred    = (is_response x)
      ; timeout = Boards.Timer.steps ~step_duration timeout
      ; exn     = None })
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
    | `Params  of int * Cstruct.t
    | `Plps    of int * Cstruct.t
    ]
  val handle_event : push_events -> event -> t -> t
  val wait         : t -> t
  val send         : t -> unit Lwt.t
  val step         : t -> t
  val responsed    : t -> event_raw list -> event option
  val update_pool  : config -> t -> t
  val empty        : t -> bool
  val last         : t -> bool
  val next         : t -> t
  val make         : config -> int list -> t
end

module Make_probes(M:M) : Probes = struct

  open Boards

  let meas_period     = Timer.steps ~step_duration:M.duration 1
  let plp_list_period = Timer.steps ~step_duration:M.duration 5
  let params_period   = Timer.steps ~step_duration:M.duration 5

  module States = struct

    type 'a state =
      { prev  : 'a option
      ; timer : int
      }

    type t =
      { measures : measures state
      ; plp_list : plp_list state
      ; params   : params   state
      }

    let empty : t =
      { measures = { timer = meas_period; prev     = None }
      ; plp_list = { timer = plp_list_period; prev = None }
      ; params   = { timer = params_period; prev   = None }
      }

  end

  type id = int
  type event_raw =
    [ `Measure of int * Cstruct.t
    | `Params  of int * Cstruct.t
    | `Plps    of int * Cstruct.t
    ]
  type t  =
    { states : (id * States.t) list
    ; pool   : (event_raw,event) Pool.t
    }

  module Timer = struct
    let reset_measures (t:States.t) : States.t =
      { t with measures = { t.measures with timer = meas_period }}
    let reset_plp_list (t:States.t) : States.t =
      { t with plp_list = { t.plp_list with timer = plp_list_period }}
    let reset_params (t:States.t)   : States.t =
      { t with params = { t.params with timer = params_period }}
  end

  let make_measure_probe id : event_msg =
    let req = Get_measure id in
    { send    = (fun () -> M.send req)
    ; pred    = is_event req
    ; timeout = M.timeout
    ; exn     = None
    }

  let make_plp_list_probe id : event_msg =
    let req = Get_plp_list id in
    { send    = (fun () -> M.send req)
    ; pred    = is_event req
    ; timeout = M.timeout
    ; exn     = None
    }

  let make_params_probe id : event_msg =
    let req = Get_params id in
    { send    = (fun () -> M.send req)
    ; pred    = is_event req
    ; timeout = M.timeout
    ; exn     = None
    }

  let wait (t:t) : t =
    let pred : 'a. 'a States.state -> 'a States.state = fun x ->
      if x.timer <= 0
      then { x with timer = 0 }
      else { x with timer = x.timer - 1 } in
    let states =
      List.map (fun (id,(x:States.t)) ->
          id, ({ measures = pred x.measures
               ; plp_list = pred x.plp_list
               ; params   = pred x.params } : States.t)) t.states
    in { t with states }

  let handle_event (pe:push_events) (e:event) (t:t) : t =
    let lock_of_measures ({ timestamp; lock; _ }:measures) : lock =
      { timestamp; lock } in
    let states = match e with
      | Measures ((id, m) as rsp) ->
         (* push event in any case *)
         pe.measure rsp;
         (* update previous field *)
         (match List.find_opt (fun (x,_) -> x = id) t.states with
          | None -> None
          | Some (id,tmr) ->
             (* push lock event only if it is first event of lock state has changed *)
             (match tmr.measures.prev with
              | Some p -> if not @@ Equal.bool p.lock m.lock
                          then pe.lock (id,lock_of_measures m)
              | None   -> pe.lock (id,lock_of_measures m));
             let tmr = { tmr with measures = { tmr.measures with prev = Some m }} in
             let tmr = Timer.reset_measures tmr in
             Some (List.Assoc.set ~eq:(=) id tmr t.states))
      | Params ((id, p) as rsp)   ->
         (match List.find_opt (fun (x, _) -> x = id) t.states with
          | None -> None
          | Some (id,tmr) ->
             (* push event only if it is different from the previous one or it is first *)
             (match tmr.params.prev with
              | Some prev ->
                 let eq = Equal.option equal_t2_params in
                 if not (eq prev.params p.params)
                 then pe.params rsp
              | None   -> pe.params rsp);
             (* update previous field *)
             let tmr = { tmr with params = { tmr.params with prev = Some p }} in
             let tmr = Timer.reset_params tmr in
             Some (List.Assoc.set ~eq:(=) id tmr t.states))
      | Plp_list ((id, l) as rsp) ->
         (match List.find_opt (fun (x,_) -> x = id) t.states with
          | Some (id, tmr) ->
             (* push event only if it is different from the previous one or it is first *)
             (match tmr.plp_list.prev with
              | Some prev ->
                 let eq = Equal.list (=) in
                 if not (eq prev.plps l.plps)
                 then pe.plp_list rsp
              | None   -> pe.plp_list rsp);
             (* update previous field *)
             let tmr = { tmr with plp_list = { tmr.plp_list with prev = Some l }} in
             let tmr = Timer.reset_plp_list tmr in
             Some (List.Assoc.set ~eq:(=) id tmr t.states)
          | None   -> None)
    in match states with Some states -> { t with states } | None -> t

  let make_pool (config:config) (states:(id * States.t) list) : pool =
    List.fold_left (fun acc (id,(tmr:States.t)) ->
        match List.Assoc.get ~eq:(=) id config with
        | None      -> []
        | Some mode ->
           let ( ^:: ) = List.cons_maybe in
           let is_t2 = equal_standard mode.standard T2 in
           let meas =
             if tmr.measures.timer > 0 then None
             else Some (make_measure_probe id)
           in
           let params =
             if (tmr.params.timer > 0) || (not is_t2) then None
             else Some (make_params_probe id)
           in
           let plps =
             if (tmr.plp_list.timer > 0) || (not is_t2) then None
             else Some (make_plp_list_probe id)
           in meas ^:: params ^:: plps ^:: acc) [] states
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

  let make config (receivers:int list) : t =
    let states = List.map (fun id -> id,States.empty) receivers in
    let pool   = make_pool config states in
    { states; pool }

end

module SM = struct

  type group =
    { measures : measures list
    }

  let wakeup_timeout t = t.pred `Timeout |> ignore

  let update_config id (mode:mode) (config:config) : config =
    List.Assoc.update
      ~eq:(=)
      ~f:(function
          | Some (os:config_item) ->
             let os = { os with standard = mode.standard } in
             Some (match mode.standard with
                   | T2 -> { os with t2 = mode.channel }
                   | T  -> { os with t  = mode.channel }
                   | C  -> { os with c  = mode.channel })
          | None   -> None)
      id config

  let send_msg (type a) sender (msg:a request) : unit Lwt.t =
    (match msg with
     | Get_devinfo -> to_devinfo_req false
     | Reset -> to_devinfo_req true
     | Set_src_id id -> to_src_id_req id
     | Set_mode (id, m) -> to_mode_req id m
     | Set_plp (id, plp) -> to_plp_set_req id plp)
    |> sender

  let send_event (type a) sender (msg:a event_request) : unit Lwt.t =
    (match msg with
     | Get_measure  id -> to_measure_req id
     | Get_params   id -> to_params_req id
     | Get_plp_list id -> to_plp_list_req id)
    |> sender

  let send (type a) state msgs sender (pe:push_events)
        timeout (msg:a request) : a Lwt.t =
    match React.S.value state with
    | `Fine ->
       let t, w = Lwt.wait () in
       let pred = function
         | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
         | l ->
            let open Option in
            is_response msg l >|= fun r ->
            (match msg with
             | Set_mode _ -> pe.mode ((fst r),(snd r).mode)
             | _ -> ());
            Lwt.wakeup w r
       in
       let send = fun () -> send_msg sender msg in
       msgs := Queue.append !msgs { send; pred; timeout; exn = None };
       t
    | _ -> Lwt.fail (Failure "board is not responding")

  let initial_timeout = -1

  let step source msgs sender (storage : config storage)
        step_duration (pe:push_events) (log_prefix:string) =

    let fmt fmt = let fs = "%s" ^^ fmt in Printf.sprintf fs log_prefix in

    let module Probes =
      Make_probes(struct
          let duration = step_duration
          let send     = send_event sender
          let timeout  = Boards.Timer.steps ~step_duration timeout
        end) in

    let rec first_step () =
      Logs.info (fun m -> m "%s" @@ fmt "start of connection establishment...");
      Queue.iter !msgs wakeup_timeout;
      msgs := Queue.create [];
      pe.state `No_response;
      let pool = Pool.create (detect_msgs (send_msg sender) step_duration) in
      Pool.send pool () |> ignore;
      `Continue (step_detect pool None)

    and step_detect detect_pool acc recvd =
      try
        let recvd = concat_acc acc recvd in
        let _, responses, acc = deserialize recvd in
        match Pool.responsed detect_pool responses with
        | Some devinfo ->
           pe.state `Init;
           pe.devinfo (Some devinfo);
           Logs.info (fun m ->
               m "%s" @@ fmt "connection established, \
                              board initialization started...");
           let req = Set_src_id source in
           let msg =
             { send = (fun () -> (send_msg sender) req)
             ; pred = (is_response req)
             ; timeout = Boards.Timer.steps ~step_duration timeout
             ; exn = None
             } in
           let pool = Pool.create [ msg ] in
           `Continue (step_init_src_id devinfo pool None)
        | _         ->
           `Continue (step_detect (Pool.step detect_pool) acc)
      with Timeout ->
        Logs.warn (fun m ->
            let s = fmt "connection is not established after %d seconds, \
                         restarting..." timeout in
            m "%s" s);
        first_step ()

    and step_init_src_id devinfo pool acc recvd =
      try
        let recvd = concat_acc acc recvd in
        let _, responses, acc = deserialize recvd in
        match Pool.responsed pool responses with
        | None ->
           `Continue (step_init_src_id devinfo (Pool.step pool) acc)
        | Some id when id = source ->
           Logs.debug (fun m ->
               m "%s" @@ fmt "source id setup done! id = %d" id);
           step_start_init devinfo
        | Some id ->
           Logs.warn (fun m ->
               m "%s" @@ fmt "failed setup source id! board returned id = %d, \
                              expected %d" id source);
           first_step ()
      with Timeout ->
        Logs.warn (fun m ->
            m "%s" @@ fmt "timeout while initializing source id, \
                           restarting...");
        first_step ()

    and step_start_init devinfo =
      let probes = Probes.make storage#get devinfo.receivers in
      match init_msgs (send_msg sender) step_duration
              storage#get devinfo.receivers with
      | [] ->
         Logs.debug (fun m ->
             m "%s" @@ fmt "nothing to initialize, skipping...");
         `Continue (step_ok_tee probes None)
      | lst ->
         Logs.debug (fun m ->
             let s = fmt "found %d receivers to be initialized..."
                     @@ List.length lst in m "%s" s);
         let init_pool = Pool.create lst in
         Pool.send init_pool () |> ignore;
         `Continue (step_init devinfo probes init_pool None)

    and step_init devinfo probes init_pool acc recvd =
      try
        let recvd = concat_acc acc recvd in
        let _, responses, acc = deserialize recvd in
        match Pool.responsed init_pool responses with
        | None ->
           `Continue (step_init devinfo probes (Pool.step init_pool) acc)
        | Some (id, mode) ->
           Logs.debug (fun m ->
               let s = show_mode_rsp mode in
               m "%s" @@ fmt "receiver #%d initialized! mode = %s" id s);
           begin match Pool.last init_pool with
           | true ->
              Logs.info (fun m -> m "%s" @@ fmt "initialization done!");
              pe.state `Fine;
              `Continue (step_ok_tee probes acc)
           | false  ->
              let init_pool = Pool.next init_pool in
              Pool.send init_pool () |> ignore;
              `Continue (step_init devinfo probes init_pool acc)
           end
      with Timeout ->
        Logs.warn (fun m ->
            m "%s" @@ fmt "timeout while initilizing receivers, restarting...");
        first_step ()

    and step_ok_tee probes acc recvd =
      match Queue.empty !msgs with
      | true  ->
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
      let probes       = Probes.wait probes in
      let recvd_buf    = concat_acc acc recvd in
      let events,_,acc = deserialize recvd_buf in
      try
        (match Probes.responsed probes events with
         | None    ->
            let probes = Probes.step probes in
            `Continue (step_ok_probes_wait probes acc)
         | Some ev ->
            Logs.debug (fun m ->
                let s = show_event ev in
                m "%s" @@ fmt "got probe response: %s" s);
            let probes = Probes.handle_event pe ev probes in
            if Probes.last probes
            then `Continue (step_ok_tee probes acc)
            else let probes = Probes.next probes in
                 `Continue (step_ok_probes_send probes acc))
      with Timeout ->
        Logs.warn (fun m ->
            m "%s" @@ fmt "timeout while waiting for probe response, \
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
      let recvd  = concat_acc acc recvd in
      let _,responses,acc = deserialize recvd in
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
            let s = fmt "timeout while waiting for client request response, \
                         restarting..." in
            m "%s" s);
        first_step ()

    in

    first_step ()

  let to_streams_s storage (e:(int * measures) React.event) =
    React.S.fold
      (fun acc (id, (m:measures)) ->
        let open Common.Stream in
        let eq = Raw.equal in
        let std, plp, freq, bw =
          List.find_map (fun (x, c) ->
              if id = x then Some c else None) storage#get
          |> Option.get_exn
          |> (fun x -> match x.standard with
                       | T2 -> T2, x.t2.plp, x.t2.freq, x.t2.bw
                       | T  -> T, 0, x.t.freq, x.t.bw
                       | C  -> C, 0, x.c.freq, x.c.bw) in
        let freq = Int64.of_int freq in
        let bw   = match bw with Bw8 -> 8. | Bw7 -> 7. | Bw6 -> 6. in
        let info : Source.t = match std with
          | T2 -> DVB_T2 { freq; bw; plp }
          | T  -> DVB_T  { freq; bw }
          | C  -> DVB_C  { freq; bw} in
        let id = Multi_TS_ID.make { source_id = 0; stream_id = id } in
        let (stream:Raw.t) =
          { source = { info; node = Port 0 }
          ; id     = TS_multi id
          ; typ    = TS
          } in
        match m.lock, m.bitrate with
        | true, Some x when x > 0 -> List.add_nodup ~eq stream acc
        | _ -> List.remove ~eq ~x:stream acc)
      [] e

  let map_measures storage (e:(int * measures) React.event)
      : (int * measures) React.event =
    React.E.map (fun (id,(m:measures)) ->
        match m.freq, List.Assoc.get ~eq:(=) id storage#get with
        | Some x, Some c ->
           let freq = match c.standard with
             | C  -> c.c.freq
             | T  -> c.t.freq
             | T2 -> c.t2.freq
           in
           id, ({ m with freq = Some (x - freq) } : measures)
        | _ -> id,{ m with freq = None }) e

  let create source (log_prefix:string) sender
        (storage:config storage) step_duration =
    let s_devinfo,devinfo_push   = React.S.create None in
    let e_mode,mode_push         = React.E.create () in
    let e_lock,lock_push         = React.E.create () in
    let e_measures,measures_push = React.E.create () in
    let e_params,params_push     = React.E.create () in
    let e_plp_list,plp_list_push = React.E.create () in
    let s_state,state_push       = React.S.create `No_response in
    let e_config =
      React.E.map (fun (id, mode) ->
          let c = update_config id mode storage#get in
          storage#store c;
          c) e_mode in
    let hold_e f e  =
      React.E.fold (fun acc x -> List.Assoc.set ~eq:(=) (f x) x acc) [] e
      |> React.E.map (List.map snd)
      |> React.S.hold [] in
    let s_lock = hold_e fst e_lock in
    let s_measures = hold_e fst e_measures in
    let s_params = hold_e fst e_params in
    let s_plp_list = hold_e fst e_plp_list in
    let (events : events)   =
      let measures = map_measures storage e_measures in
      { mode     = e_mode
      ; lock     = e_lock
      ; measures
      ; params   = e_params
      ; plp_list = e_plp_list
      ; devinfo  = s_devinfo
      ; config   = e_config
      ; state    = s_state
      ; streams  = to_streams_s storage measures
      } in
    let (push_events : push_events) =
      { mode     = mode_push
      ; lock     = lock_push
      ; measure  = measures_push
      ; params   = params_push
      ; state    = state_push
      ; plp_list = plp_list_push
      ; devinfo  = devinfo_push
      } in
    let msgs    = ref (Queue.create []) in
    let steps   = Boards.Timer.steps ~step_duration timeout in
    let send x  = send s_state msgs sender push_events steps x in
    let fmt fmt = let fs = "%s" ^^ fmt in Printf.sprintf fs log_prefix in
    let api : api  =
      { get_devinfo  = (fun () -> Lwt.return @@ React.S.value s_devinfo)
      ; reset        = (fun () ->
        Logs.info (fun m -> m "%s" @@ fmt "got reset request");
        send Reset)
      ; set_mode     = (fun r  ->
        Logs.info (fun m ->
            let s = fmt "got set mode request for receiver #%d: %s"
                      (fst r) (show_mode (snd r)) in
            m "%s" s);
        send (Set_mode r))
      ; get_config   = (fun () -> storage#get)
      ; get_lock     = (fun () -> React.S.value s_lock)
      ; get_measures = (fun () -> React.S.value s_measures)
      ; get_params   = (fun () -> React.S.value s_params)
      ; get_plp_list = (fun () -> React.S.value s_plp_list)
      }
    in
    events,
    api,
    (step source msgs sender storage step_duration push_events log_prefix)

end
