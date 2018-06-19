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
  { measures : measures React.event
  ; plp_list : int list React.event
  }

type events =
  { device   : device_events
  ; receiver : receiver_events
  }

type api =
  { get_devinfo  : unit -> devinfo_opt Lwt.t
  ; reset        : unit -> unit Lwt.t
  ; set_mode     : mode_req -> mode_rsp Lwt.t
  ; set_plp      : plp_set_req -> plp_set_rsp Lwt.t
  ; get_config   : unit -> config Lwt.t
  }

let ( % ) = Fun.(%)

(* Board protocol implementation *)

let to_period sec step_duration  = sec * int_of_float (1. /. step_duration)

let probe_period step_duration   = to_period 2 step_duration
let request_period step_duration = to_period 3 step_duration

let detect = Get_devinfo

let detect_msgs (send_req : 'a request -> unit Lwt.t) timeout =
  [ { send = (fun () -> send_req detect)
    ; pred = (is_response detect)
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

type ev      = [ `Measure of int * Cstruct.t | `Plps of int * Cstruct.t ]
type ev_msg  = (ev,event) msg
type ev_msgs = (ev,event) msg list

let make_measure_probe (send : 'a event_request -> unit Lwt.t) timeout id : ev_msg =
  let req = Get_measure id in
  { send    = (fun () -> send req)
  ; pred    = is_event req
  ; timeout
  ; exn     = None
  }

let make_plp_list_probe (send : 'a event_request -> unit Lwt.t) timeout id : ev_msg =
  let req = Get_plp_list id in
  { send    = (fun () -> send req)
  ; pred    = is_event req
  ; timeout
  ; exn     = None
  }

module type M = sig
  val duration : float
  val timeout  : int
  val send     : event event_request -> unit Lwt.t
end

module Make_probes(M:M) = struct

  module Timer = struct

    let meas_period     = to_period 2 M.duration
    let plp_list_period = to_period 5 M.duration
    let params_period   = to_period 5 M.duration

    type timer = { measures : int
                 ; plp_list : int
                 ; params   : int
                 }

    type t = (int * timer) list

    let empty : timer =
      { measures = meas_period
      ; plp_list = plp_list_period
      ; params   = params_period
      }

    let of_devinfo : devinfo -> t = fun di ->
      List.map (fun id -> id,empty) di.receivers

    let to_receivers : t -> int list = List.map fst

    let step (t:t) : t =
      let pred x = if x <= 0 then 0 else x - 1 in
      List.map (fun (id,x) -> id,{ measures = pred x.measures
                                 ; plp_list = pred x.plp_list
                                 ; params   = pred x.params }) t

  end

  let make_pool (config:config) (t:Timer.t) : (ev,event) Pool.t * Timer.t =
    let (^::) = List.cons_maybe in
    List.fold_left (fun (acc,t) (id,(tmr:Timer.timer)) ->
        match List.Assoc.get ~eq:(=) id config with
        | None      -> [],t
        | Some mode ->
           let meas,tmr = if tmr.measures <= 0
                          then Some (make_measure_probe M.send M.timeout id),
                               { tmr with measures = Timer.meas_period }
                          else None, tmr
           in
           let plps,tmr = if (tmr.plp_list <= 0)
                             && (equal_standard mode.standard T2)
                          then Some (make_plp_list_probe M.send M.timeout id),
                               { tmr with plp_list = Timer.plp_list_period }
                          else None, tmr
           in
           (meas ^:: plps ^:: acc, (id,tmr) :: t)) ([],[]) t
    |> Pair.map1 Pool.create

end

module SM = struct

  type group =
    { measures : measures list
    }

  let wakeup_timeout t = t.pred `Timeout |> ignore

  type push_events = { measure  : measures -> unit
                     ; state    : Common.Topology.state -> unit
                     ; plp_list : plp_list -> unit
                     ; devinfo  : devinfo_opt -> unit
                     }

  let update_config (rsp:mode_rsp) (config:config) : config =
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

  let event_push (pe:push_events) = function
    | Measures x -> pe.measure x
    | Plp_list x -> pe.plp_list x

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
    | Get_measure id  -> sender @@ to_measure_req id
    | Get_plp_list id -> sender @@ to_plp_list_req id

  let send (type a) msgs sender (storage : config storage) timeout (msg : a request) : a Lwt.t =
    (* no instant msgs *)
    let t, w = Lwt.wait () in
    let pred = function
      | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
      | l -> let open Option in
             is_response msg l >|= fun rsp ->
             (match msg with
              | Set_mode _ -> storage#store (update_config rsp storage#get);
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

    and step_ok_tee timers acc recvd =
      match Queue.empty !msgs with
      | true  ->
         let pool,timers = Probes.make_pool storage#get timers in
         step_ok_probes_send timers pool acc recvd
      | false -> step_ok_requests_send timers acc recvd

    and step_start_init devinfo =
      let timers = Probes.Timer.of_devinfo devinfo in
      match init_msgs (send_msg sender) period storage#get devinfo.receivers with
      | []  ->
         Logs.debug (fun m -> m "%s" @@ fmt "nothing to initialize, skipping...");
         `Continue (step_ok_tee timers None)
      | lst ->
         Logs.debug (fun m ->
             let s = Printf.sprintf "found %d receivers to be initialized..." @@ List.length lst in
             m "%s" @@ fmt s);
         let init_pool = Pool.create lst in
         Pool.send init_pool () |> ignore;
         `Continue (step_init devinfo timers init_pool None)

    and step_init devinfo timers init_pool acc recvd =
      try
        let recvd = concat_acc acc recvd in
        let _, responses, acc = deserialize recvd in
        match Pool.responsed init_pool responses with
        | None     ->
           `Continue (step_init devinfo timers (Pool.step init_pool) acc)
        | Some rsp ->
           Logs.debug (fun m -> m "%s" @@ fmt @@ Printf.sprintf "receiver #%d initialized!" rsp.id);
           storage#store (update_config rsp storage#get);
           (match Pool.last init_pool with
            | true ->
               Logs.info (fun m -> m "%s" @@ fmt "initialization done!");
               push_events.state `Fine;
               `Continue (step_ok_tee timers acc)
            | false  ->
               let init_pool = Pool.next init_pool in
               Pool.send init_pool () |> ignore;
               `Continue (step_init devinfo timers init_pool acc))
      with Timeout ->
        Logs.warn (fun m ->
            let s = Printf.sprintf "timeout while initilizing receivers, restarting..." in
            m "%s" @@ fmt s);
        first_step ()

    and step_ok_probes_send timers probes_pool acc _ =
      let timers = Probes.Timer.step timers in
      if Pool.empty probes_pool
      then `Continue (step_ok_tee timers acc)
      else (Pool.send probes_pool () |> ignore;
            `Continue (step_ok_probes_wait timers probes_pool acc))

    and step_ok_probes_wait timers probes_pool acc recvd =
      let timers       = Probes.Timer.step timers in
      let recvd_buf    = concat_acc acc recvd in
      let events,_,acc = deserialize recvd_buf in
      try
        (match Pool.responsed probes_pool events with
         | None    ->
            let probes_pool = Pool.step probes_pool in
            `Continue (step_ok_probes_wait timers probes_pool acc)
         | Some ev ->
            let new_probes_pool = Pool.next probes_pool in
            event_push push_events ev;
            if Pool.last probes_pool
            then `Continue (step_ok_requests_send timers acc)
            else step_ok_probes_send timers new_probes_pool acc recvd)
      with Timeout -> first_step ()

    and step_ok_requests_send timers acc _ =
      let timers = Probes.Timer.step timers in
      if Queue.empty !msgs
      then `Continue (step_ok_tee timers acc)
      else (Queue.send !msgs () |> ignore;
            `Continue (step_ok_requests_wait timers acc))

    and step_ok_requests_wait timers acc recvd =
      let timers = Probes.Timer.step timers in
      let recvd  = concat_acc acc recvd in
      let _,responses,acc = deserialize recvd in
      try
        match Queue.responsed !msgs responses with
        | None    ->
           msgs := Queue.step !msgs;
           `Continue (step_ok_requests_wait timers acc)
        | Some () ->
           msgs := Queue.next !msgs;
           `Continue (step_ok_requests_send timers acc)
      with Timeout -> first_step ()

    in

    first_step ()

  let to_streams_s = ()

  let create (fmt:string -> string) sender (storage : config storage) step_duration =
    let s_config, s_config_push   = React.S.create storage#get in
    let s_devinfo, s_devinfo_push = React.S.create None in
    let e_measure, e_measure_push = React.E.create () in
    let s_state,state_push = React.S.create `No_response in
    let (events : events)   =
      { receiver = { measures = React.E.map (fun m ->
                                    match m.freq, List.Assoc.get ~eq:(=) m.id (React.S.value s_config) with
                                    | Some x,Some c -> let freq = match c.standard with
                                                         | C  -> c.c.freq
                                                         | T  -> c.t.freq
                                                         | T2 -> c.t2.freq
                                                       in
                                                       { m with freq = Some (x - freq) }
                                    | _             -> { m with freq = None }) e_measure
                   ; plp_list = fst @@ React.E.create ()
                   }
      ; device   = { devinfo = s_devinfo
                   ; config  = React.S.changes s_config
                   ; state   = s_state
                   }
      }
    in
    let (push_events : push_events) =
      { measure  = e_measure_push
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
