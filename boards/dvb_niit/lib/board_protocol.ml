open Containers
open Board_types
open Lwt.Infix
open Storage.Options
open Api.Handler
open Boards.Board
open Boards.Pools

include Board_parser

type device_events =
  { config  : config React.event
  ; devinfo : devinfo_opt React.signal
  ; state   : Common.Topology.state React.signal
  }

type receiver_events =
  { measures : measures      React.event
  ; params   : t2_params_rsp React.event
  ; plp_list : plp_list_rsp  React.event
  }

type events =
  { device   : device_events
  ; receiver : receiver_events
  }

type push_events =
  { measure  : measures -> unit
  ; params   : t2_params_rsp -> unit
  ; state    : Common.Topology.state -> unit
  ; plp_list : plp_list_rsp -> unit
  ; devinfo  : devinfo_opt -> unit
  }

type api =
  { get_devinfo  : unit        -> devinfo_opt Lwt.t
  ; reset        : unit        -> unit Lwt.t
  ; set_mode     : mode_req    -> mode_rsp Lwt.t
  ; set_plp      : plp_set_req -> plp_set_rsp Lwt.t
  ; get_config   : unit        -> config Lwt.t
  }

let ( % ) = Fun.(%)

(* Board protocol implementation *)

let to_period sec step_duration  = sec * int_of_float (1. /. step_duration)

let probe_period step_duration   = to_period 2 step_duration
let request_period step_duration = to_period 3 step_duration

let detect_msgs (send_req : 'a request -> unit Lwt.t) timeout =
  let req = Get_devinfo in
  [ { send = (fun () -> send_req req)
    ; pred = (is_response req)
    ; timeout
    ; exn = None
  } ]


let init =
  let get_mode x = match x.standard with
    | T2 -> { standard = T2; channel = x.t2 }
    | T  -> { standard = T ; channel = x.t  }
    | C  -> { standard = C ; channel = x.c  }
  in
  List.map (fun (id,x) -> Set_mode { id; mode = get_mode x })

let init_msgs (send_req  : 'a request -> unit Lwt.t)
              (timeout   : int)
              (config    : config)
              (receivers : int list) =
  let config = List.map (fun id ->
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
           (init config)

type event_raw = [ `Measure of int * Cstruct.t
                 | `Params of int * Cstruct.t
                 | `Plps of int * Cstruct.t
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
      ; plp_list : plp_list_rsp state
      ; params   : t2_params_rsp state
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
           let tmr = { tmr with measures = { tmr.measures with prev = Some rsp }} in
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
           List.Assoc.set ~eq:(=) id tmr t
        | None   -> t)

  let make_pool (config:config) (t:t) : pool * t =
    let open Timer in
    List.fold_left (fun (acc,t) (id,(tmr:States.t)) ->
        match List.Assoc.get ~eq:(=) id config with
        | None      -> [],t
        | Some mode ->
           let (^::) = List.cons_maybe in
           let is_t2 = equal_standard mode.standard T2 in
           let meas,tmr =
             if tmr.measures.timer > 0 then None,tmr
             else Some (make_measure_probe id),reset_measures tmr
           in
           let params,tmr =
             if (tmr.params.timer > 0) || (not is_t2) then None,tmr
             else Some (make_params_probe id),reset_params tmr
           in
           let plps,tmr =
             if (tmr.plp_list.timer > 0) || (not is_t2) then None,tmr
             else Some (make_plp_list_probe id),reset_plp_list tmr
           in (meas ^:: params ^:: plps ^:: acc, (id,tmr) :: t)) ([],[]) t
    |> Pair.map1 Pool.create

end

module SM = struct

  type group =
    { measures : measures list
    }

  let wakeup_timeout t = t.pred `Timeout |> ignore

  let update_config_mode (rsp:mode_rsp) (config:config) : config =
    List.Assoc.update
      ~eq:(=)
      ~f:(function
          | Some (os:config_item) ->
             let os = { os with standard = rsp.mode.standard } in
             Some (match rsp.mode.standard with
                   | T2 -> { os with t2 = rsp.mode.channel }
                   | T  -> { os with t  = rsp.mode.channel }
                   | C  -> { os with c  = rsp.mode.channel })
          | None   -> None)
      rsp.id config

  let update_config_plp (rsp:plp_set_rsp) (config:config) : config =
    List.Assoc.update
      ~eq:(=)
      ~f:(function
          | Some (os:config_item) ->
             let upd_plp ch : channel = { ch with plp = rsp.plp } in
             Some (match os.standard with
                   | T2 -> { os with t2 = upd_plp os.t2 }
                   | T  -> { os with t  = upd_plp os.t  }
                   | C  -> { os with c  = upd_plp os.c  })
          | None   -> None)
      rsp.id config

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

  let send (type a) msgs sender (storage : config storage) timeout (msg : a request) : a Lwt.t =
    (* no instant msgs *)
    let t, w = Lwt.wait () in
    let pred = function
      | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
      | l -> let open Option in
             is_response msg l >|= fun rsp ->
             (match msg with
              | Set_mode _ -> storage#store (update_config_mode rsp storage#get)
              | Set_plp  _ -> storage#store (update_config_plp rsp storage#get)
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
         let pool,probes = Probes.make_pool storage#get probes in
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
           Logs.debug (fun m -> m "%s" @@ fmt @@ Printf.sprintf "receiver #%d initialized!" rsp.id);
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

  let map_measures s_config e =
    React.E.map (fun (m:measures) ->
        match m.freq, List.Assoc.get ~eq:(=) m.id (React.S.value s_config) with
        | Some x,Some c -> let freq = match c.standard with
                             | C  -> c.c.freq
                             | T  -> c.t.freq
                             | T2 -> c.t2.freq
                           in
                           ({ m with freq = Some (x - freq) }:measures)
        | _             -> { m with freq = None }) e

  let create (fmt:string -> string) sender (storage : config storage) step_duration =
    let s_config, s_config_push   = React.S.create storage#get in
    let s_devinfo, s_devinfo_push = React.S.create None in
    let e_measure, e_measure_push = React.E.create () in
    let e_params, params_push     = React.E.create () in
    let s_state,state_push = React.S.create `No_response in
    let receiver_events =
      { measures = map_measures s_config e_measure
      ; params   = e_params
      ; plp_list = fst @@ React.E.create ()
      }
    in
    let (events : events)   =
      { receiver = receiver_events
      ; device   = { devinfo = s_devinfo
                   ; config  = React.S.changes s_config
                   ; state   = s_state
                   }
      }
    in
    let (push_events : push_events) =
      { measure  = e_measure_push
      ; params   = params_push
      ; state    = state_push
      ; plp_list = snd @@ React.E.create ()
      ; devinfo  = s_devinfo_push
      }
    in
    let msgs = ref (Queue.create []) in
    let send x = send msgs sender storage (request_period step_duration) x in
    let api : api  =
      { get_devinfo = (fun ()  -> Lwt.return @@ React.S.value s_devinfo)
      ; reset       = (fun ()  -> send Reset)
      ; set_mode    = (fun req -> send (Set_mode req)
                                  >>= (fun x -> s_config_push storage#get; Lwt.return x))
      ; set_plp     = (fun req -> send (Set_plp req))
      ; get_config  = (fun ()  -> Lwt.return storage#get)
      }
    in
    events,
    api,
    (step msgs sender storage step_duration push_events fmt)

end
