open Containers
open Board_types
open Lwt.Infix
open Storage.Options
open Api.Handler
open Boards.Board
open Boards.Pools

include Board_parser

type events =
  { config   : config React.event
  ; mode     : mode React.event
  ; devinfo  : devinfo_opt React.signal
  ; state    : Common.Topology.state React.signal
  ; lock     : lock React.event
  ; measures : measures React.event
  ; params   : params React.event
  ; plp_list : plp_list React.event
  }

type push_events =
  { mode     : mode                  -> unit
  ; lock     : lock                  -> unit
  ; measure  : measures              -> unit
  ; params   : params                -> unit
  ; state    : Common.Topology.state -> unit
  ; plp_list : plp_list              -> unit
  ; devinfo  : devinfo_opt           -> unit
  }

type api =
  { get_devinfo  : unit -> devinfo_opt Lwt.t
  ; reset        : unit -> unit Lwt.t
  ; set_mode     : mode -> mode_rsp Lwt.t
  ; get_config   : unit -> config
  ; get_lock     : unit -> lock list
  ; get_measures : unit -> measures list
  ; get_params   : unit -> params list
  ; get_plp_list : unit -> plp_list list
  }

(* Board protocol implementation *)

let to_period sec step_duration  = sec * int_of_float (1. /. step_duration)
let request_period step_duration = to_period 3 step_duration

let detect_msgs (send_req : 'a request -> unit Lwt.t) timeout =
  let req = Get_devinfo in
  [ { send = (fun () -> send_req req)
    ; pred = (is_response req)
    ; timeout
    ; exn = None
  } ]

let init_requests =
  let get_mode id x = match x.standard with
    | T2 -> { id; standard = T2; channel = x.t2 }
    | T  -> { id; standard = T ; channel = x.t  }
    | C  -> { id; standard = C ; channel = x.c  }
  in
  List.map (fun (id,x) -> Set_mode (get_mode id x))

let init_msgs (send_req  : 'a request -> unit Lwt.t)
              (timeout   : int)
              (config    : config)
              (receivers : int list) =
  (* If config for a receiver is empty, create default one *)
  let config =
    List.map (fun id ->
        match List.find_opt (fun (x,_) -> id = x) config with
        | Some x -> x
        | None   -> id,{ standard = T2
                       ; t2       = default ()
                       ; t        = default ()
                       ; c        = default () }) receivers in
  List.map (fun x -> { send = (fun () -> send_req x)
                     ; pred = (is_response x)
                     ; timeout
                     ; exn = None })
           (init_requests config)

type event_raw = [ `Measure of int * Cstruct.t
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

module Make_probes(M:M) = struct

  let meas_period     = to_period 2 M.duration
  let plp_list_period = to_period 5 M.duration
  let params_period   = to_period 5 M.duration

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
  type t  = (id * States.t) list

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

  let lock_of_measures ({id;timestamp;lock;_}:measures) : lock = {id;timestamp;lock}

  let of_devinfo : devinfo -> t = fun di ->
    List.map (fun id -> id,States.empty) di.receivers

  let step (t:t) : t =
    let pred : 'a. 'a States.state -> 'a States.state = fun x ->
      if x.timer <= 0 then { x with timer = 0 } else { x with timer = x.timer - 1 } in
    List.map (fun (id,(x:States.t)) -> id,({ measures = pred x.measures
                                           ; plp_list = pred x.plp_list
                                           ; params   = pred x.params }:States.t)) t

  let push (pe:push_events) (t:t) (e:event) : t =
    match e with
    | Measures rsp ->
       (* push event in any case *)
       pe.measure rsp;
       (* update previous field *)
       (match List.find_opt (fun (id,_) -> id = rsp.id) t with
        | Some (id,tmr) ->
           (* push lock event only if it is first event of lock state has changed *)
           (match tmr.measures.prev with
            | Some p -> if not @@ Equal.bool p.lock rsp.lock
                        then pe.lock @@ lock_of_measures rsp
            | None   -> pe.lock @@ lock_of_measures rsp);
           let tmr = { tmr with measures = { tmr.measures with prev = Some rsp }} in
           let tmr = Timer.reset_measures tmr in
           List.Assoc.set ~eq:(=) id tmr t
        | None   -> t)
    | Params rsp   ->
       (match List.find_opt (fun (id,_) -> id = rsp.id) t with
        | Some (id,tmr) ->
           (* push event only if it is different from the previous one or it is first *)
           (match tmr.params.prev with
            | Some p ->
               let eq = Equal.option equal_t2_params in
               if not (eq p.params rsp.params)
               then pe.params rsp
            | None   -> pe.params rsp);
           (* update previous field *)
           let tmr = { tmr with params = { tmr.params with prev = Some rsp }} in
           let tmr = Timer.reset_params tmr in
           List.Assoc.set ~eq:(=) id tmr t
        | None   -> t)
    | Plp_list rsp ->
       (match List.find_opt (fun (id,_) -> id = rsp.id) t with
        | Some (id,tmr) ->
           (* push event only if it is different from the previous one or it is first *)
           (match tmr.plp_list.prev with
            | Some p ->
               let eq = Equal.list (=) in
               if not (eq p.plps rsp.plps)
               then pe.plp_list rsp
            | None   -> pe.plp_list rsp);
           (* update previous field *)
           let tmr = { tmr with plp_list = { tmr.plp_list with prev = Some rsp }} in
           let tmr = Timer.reset_plp_list tmr in
           List.Assoc.set ~eq:(=) id tmr t
        | None   -> t)

  let make_pool (config:config) (t:t) : pool =
    let open Timer in
    List.fold_left (fun acc (id,(tmr:States.t)) ->
        match List.Assoc.get ~eq:(=) id config with
        | None      -> []
        | Some mode ->
           let (^::) = List.cons_maybe in
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
           in (meas ^:: params ^:: plps ^:: acc)) [] t
    |> Pool.create

end

module SM = struct

  type group =
    { measures : measures list
    }

  let wakeup_timeout t = t.pred `Timeout |> ignore

  let update_config (mode:mode) (config:config) : config =
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
      mode.id config

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    (match msg with
     | Get_devinfo     -> to_devinfo_req false
     | Reset           -> to_devinfo_req true
     | Set_mode req    -> to_mode_req req
     | Set_plp req     -> to_plp_set_req req)
    |> sender

  let send_event (type a) sender (msg : a event_request) : unit Lwt.t =
    (* no instant msgs *)
    match msg with
    | Get_measure  id -> sender @@ to_measure_req  id
    | Get_params   id -> sender @@ to_params_req   id
    | Get_plp_list id -> sender @@ to_plp_list_req id

  let send (type a) msgs sender (pe:push_events) timeout (msg : a request) : a Lwt.t =
    (* no instant msgs *)
    let t, w = Lwt.wait () in
    let pred = function
      | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
      | l -> let open Option in
             is_response msg l >|= fun rsp ->
             (match msg with
              | Set_mode _ -> pe.mode rsp.mode
              | _ -> ());
             Lwt.wakeup w rsp
    in
    let send = fun () -> send_msg sender msg in
    msgs := Queue.append !msgs { send; pred; timeout; exn = None };
    t

  let initial_timeout = -1

  let step msgs sender (storage : config storage) step_duration push_events (fmt:string -> string) =
    let period      = request_period step_duration in
    let detect_pool = Pool.create (detect_msgs (send_msg sender) period) in

    let module Probes = Make_probes(struct
                                     let duration = step_duration
                                     let send     = send_event sender
                                     let timeout  = period
                                   end) in

    let rec first_step () =
      Logs.info (fun m -> m "%s" @@ fmt "start of connection establishment...");
      Queue.iter !msgs wakeup_timeout;
      msgs := Queue.create [];
      push_events.state `No_response;
      Pool.send detect_pool () |> ignore;
      `Continue (step_detect detect_pool None)

    and step_detect detect_pool acc recvd =
      try
        let recvd = concat_acc acc recvd in
        let _, responses, acc = deserialize recvd in
        match Pool.responsed detect_pool responses with
        | Some devinfo ->
           push_events.state `Init;
           push_events.devinfo (Some devinfo);
           Logs.info (fun m -> m "%s" @@ fmt "connection established, board initialization started...");
           step_start_init devinfo
        | _         ->
           `Continue (step_detect (Pool.step detect_pool) acc)
      with Timeout ->
        Logs.warn (fun m ->
            let s = Printf.sprintf "connection is not established after %d seconds, restarting..." period in
            m "%s" @@ fmt s);
        first_step ()

    and step_ok_tee probes acc recvd =
      match Queue.empty !msgs with
      | true  ->
         let pool = Probes.make_pool storage#get probes in
         step_ok_probes_send probes pool acc recvd
      | false -> step_ok_requests_send probes acc recvd

    and step_start_init devinfo =
      let probes = Probes.of_devinfo devinfo in
      match init_msgs (send_msg sender) period storage#get devinfo.receivers with
      | []  ->
         Logs.debug (fun m -> m "%s" @@ fmt "nothing to initialize, skipping...");
         `Continue (step_ok_tee probes None)
      | lst ->
         Logs.debug (fun m ->
             let s = Printf.sprintf "found %d receivers to be initialized..." @@ List.length lst in
             m "%s" @@ fmt s);
         let init_pool = Pool.create lst in
         Pool.send init_pool () |> ignore;
         `Continue (step_init devinfo probes init_pool None)

    and step_init devinfo probes init_pool acc recvd =
      try
        let recvd = concat_acc acc recvd in
        let _, responses, acc = deserialize recvd in
        match Pool.responsed init_pool responses with
        | None     ->
           `Continue (step_init devinfo probes (Pool.step init_pool) acc)
        | Some rsp ->
           Logs.debug (fun m ->
               let s = show_mode_rsp rsp in
               m "%s" @@ fmt @@ Printf.sprintf "receiver #%d initialized! mode = %s" rsp.mode.id s);
           (match Pool.last init_pool with
            | true ->
               Logs.info (fun m -> m "%s" @@ fmt "initialization done!");
               push_events.state `Fine;
               `Continue (step_ok_tee probes acc)
            | false  ->
               let init_pool = Pool.next init_pool in
               Pool.send init_pool () |> ignore;
               `Continue (step_init devinfo probes init_pool acc))
      with Timeout ->
        Logs.warn (fun m ->
            let s = Printf.sprintf "timeout while initilizing receivers, restarting..." in
            m "%s" @@ fmt s);
        first_step ()

    and step_ok_probes_send probes probes_pool acc _ =
      let probes = Probes.step probes in
      if Pool.empty probes_pool
      then `Continue (step_ok_tee probes acc)
      else (Pool.send probes_pool () |> ignore;
            `Continue (step_ok_probes_wait probes probes_pool acc))

    and step_ok_probes_wait probes probes_pool acc recvd =
      let probes       = Probes.step probes in
      let recvd_buf    = concat_acc acc recvd in
      let events,_,acc = deserialize recvd_buf in
      try
        (match Pool.responsed probes_pool events with
         | None    ->
            let probes_pool = Pool.step probes_pool in
            `Continue (step_ok_probes_wait probes probes_pool acc)
         | Some ev ->
            Logs.debug (fun m ->
                let s = show_event ev in
                m "%s" @@ fmt @@ Printf.sprintf "got probe response: %s" s);
            let new_probes_pool = Pool.next probes_pool in
            let probes = Probes.push push_events probes ev in
            if Pool.last probes_pool
            then `Continue (step_ok_requests_send probes acc)
            else step_ok_probes_send probes new_probes_pool acc recvd)
      with Timeout ->
        Logs.warn (fun m ->
            let s = Printf.sprintf "timeout while waiting for probe response, restarting..." in
            m "%s" @@ fmt s);
        first_step ()

    and step_ok_requests_send probes acc _ =
      let probes = Probes.step probes in
      if Queue.empty !msgs
      then `Continue (step_ok_tee probes acc)
      else (Queue.send !msgs () |> ignore;
            `Continue (step_ok_requests_wait probes acc))

    and step_ok_requests_wait probes acc recvd =
      let probes = Probes.step probes in
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
            let s = Printf.sprintf "timeout while waiting for client request response, restarting..." in
            m "%s" @@ fmt s);
        first_step ()

    in

    first_step ()

  let to_streams_s = ()

  let map_measures storage e =
    React.E.map (fun (m:measures) ->
        match m.freq, List.Assoc.get ~eq:(=) m.id storage#get with
        | Some x,Some c -> let freq = match c.standard with
                             | C  -> c.c.freq
                             | T  -> c.t.freq
                             | T2 -> c.t2.freq
                           in
                           ({ m with freq = Some (x - freq) }:measures)
        | _             -> { m with freq = None }) e

  let create (fmt:string -> string) sender (storage : config storage) step_duration =
    let s_devinfo,devinfo_push   = React.S.create None in
    let e_mode,mode_push         = React.E.create () in
    let e_lock,lock_push         = React.E.create () in
    let e_measures,measures_push = React.E.create () in
    let e_params,params_push     = React.E.create () in
    let e_plp_list,plp_list_push = React.E.create () in
    let s_state,state_push       = React.S.create `No_response in
    let e_config    = React.E.map (fun (m:mode) ->
                          let c = update_config m storage#get in
                          storage#store c;
                          c) e_mode
    in
    let hold_e f e  = React.E.fold (fun acc x -> List.Assoc.set ~eq:(=) (f x) x acc) [] e
                      |> React.E.map (List.map snd)
                      |> React.S.hold []
    in
    let s_lock      = hold_e (fun (x:lock)     -> x.id) e_lock in
    let s_measures  = hold_e (fun (m:measures) -> m.id) e_measures in
    let s_params    = hold_e (fun (p:params)   -> p.id) e_params in
    let s_plp_list  = hold_e (fun (p:plp_list) -> p.id) e_plp_list in
    let (events : events)   =
      { mode     = e_mode
      ; lock     = e_lock
      ; measures = map_measures storage e_measures
      ; params   = e_params
      ; plp_list = e_plp_list
      ; devinfo  = s_devinfo
      ; config   = e_config
      ; state    = s_state
      }
    in
    let (push_events : push_events) =
      { mode     = mode_push
      ; lock     = lock_push
      ; measure  = measures_push
      ; params   = params_push
      ; state    = state_push
      ; plp_list = plp_list_push
      ; devinfo  = devinfo_push
      }
    in
    let msgs = ref (Queue.create []) in
    let send x = send msgs sender push_events (request_period step_duration) x in
    let api : api  =
      { get_devinfo  = (fun () -> Lwt.return @@ React.S.value s_devinfo)
      ; reset        = (fun () -> send Reset)
      ; set_mode     = (fun r  -> send (Set_mode r))
      ; get_config   = (fun () -> storage#get)
      ; get_lock     = (fun () -> React.S.value s_lock)
      ; get_measures = (fun () -> React.S.value s_measures)
      ; get_params   = (fun () -> React.S.value s_params)
      ; get_plp_list = (fun () -> React.S.value s_plp_list)
      }
    in
    events,
    api,
    (step msgs sender storage step_duration push_events fmt)

end
