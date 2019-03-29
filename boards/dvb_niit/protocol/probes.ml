open Board_dvb_types
open Application_types
open Boards.Util

let ( ^:: ) x l = match x with
  | None -> l
  | Some x -> x :: l

(* TODO remove from here. *)
type push_events =
  { mode : int * Device.mode -> unit
  ; measure : (int * Measure.t ts) list -> unit
  ; params : (int * Params.t ts) list -> unit
  ; plp_list : (int * Plp_list.t ts) list -> unit
  ; state : Topology.state -> unit
  ; devinfo : Device.info option -> unit
  }

module type M = sig
  val duration : float
  val timeout : int
  val send : Parser.event Parser.event_request -> unit Lwt.t
end

module type Probes = sig
  type t
  type event_raw =
    [ `Measure of int * Cstruct.t
    | `Params of int * Cstruct.t
    | `Plps of int * Cstruct.t
    ]
  val cons_event : t -> Parser.event -> t
  val handle_events : push_events -> t -> t
  val wait : t -> t
  val send : t -> unit Lwt.t
  val step : t -> t
  val responsed : t -> event_raw list -> Parser.event option
  val update_pool : (int * Device.standard) list -> t -> t
  val empty : t -> bool
  val last : t -> bool
  val next : t -> t
  val make : (int * Device.standard) list -> int list -> t
end

module Make(M : M) : Probes = struct

  open Boards

  module States = struct

    type 'a state =
      { prev : 'a option
      ; timer : int
      ; period : int
      }

    type t =
      { measures : Measure.t ts state
      ; plp_list : Plp_list.t ts state
      ; params : Params.t ts state
      }

    let empty : t =
      let meas_period = Timer.steps ~step_duration:M.duration 1. in
      let plps_period = Timer.steps ~step_duration:M.duration 5. in
      let prms_period = Timer.steps ~step_duration:M.duration 5. in
      { measures =
          { timer = meas_period
          ; period = meas_period
          ; prev = None
          }
      ; plp_list =
          { timer = plps_period
          ; period = plps_period
          ; prev = None
          }
      ; params =
          { timer = prms_period
          ; period = prms_period
          ; prev = None
          }
      }
  end

  type id = int
  type event_raw =
    [ `Measure of int * Cstruct.t
    | `Params of int * Cstruct.t
    | `Plps of int * Cstruct.t
    ]
  type event_msg = (event_raw, Parser.event) Boards.Pools.msg
  type pool = (event_raw, Parser.event) Boards.Pools.Pool.t
  type t  =
    { states : (id * States.t) list
    ; pool : (event_raw, Parser.event) Pools.Pool.t
    ; acc : acc
    }
  and acc =
    { measures : (int * Measure.t ts) list
    ; params : (int * Params.t ts) list
    ; plps : (int * Plp_list.t ts) list
    }

  let (empty_acc : acc) =
    { measures = []
    ; params = []
    ; plps = []
    }

  module Timer = struct

    let reset : 'a. 'a States.state -> 'a States.state =
      fun state -> { state with timer = state.period }

    let set_prev : 'a. 'a States.state -> 'a -> 'a States.state =
      fun state prev -> { state with prev = Some prev }

  end

  let make_measure_probe id : event_msg =
    let req = Parser.Get_measure id in
    { send = (fun () -> M.send req)
    ; pred = Parser.is_event req
    ; timeout = M.timeout
    ; exn = None
    }

  let make_plp_list_probe id : event_msg =
    let req = Parser.Get_plp_list id in
    { send = (fun () -> M.send req)
    ; pred = Parser.is_event req
    ; timeout = M.timeout
    ; exn = None
    }

  let make_params_probe id : event_msg =
    let req = Parser.Get_params id in
    { send = (fun () -> M.send req)
    ; pred = Parser.is_event req
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

  let cons_event (t : t) (event : Parser.event) : t =
    let stamp : 'a. 'a -> 'a ts = fun data ->
      { timestamp = Ptime_clock.now (); data } in
    let set id v lst = List.Assoc.set ~eq:(=) id (stamp v) lst in
    let acc = match event with
      | Measures (id, m) ->
         let measures = set id m t.acc.measures in
         { t.acc with measures }
      | Params (id, p) ->
         let params = set id p t.acc.params in
         { t.acc with params }
      | Plp_list (id, p) ->
         let plps = set id p t.acc.plps in
         { t.acc with plps } in
    { t with acc }

  let handle_measurements (pe : push_events) (t : t) : t =
    (* Push measures in any case *)
    pe.measure t.acc.measures;
    let states =
      List.fold_left (fun states (id, m) ->
          match List.assoc_opt id states with
          | None -> states
          | Some (tmr : States.t) ->
             let measures = Timer.(reset @@ set_prev tmr.measures m) in
             let tmr = { tmr with measures } in
             List.Assoc.set ~eq:(=) id tmr states)
        t.states t.acc.measures in
    { t with states }

  let handle_params (pe : push_events) (t : t) : t =
    let states, acc =
      List.fold_left (fun (states, acc)
                          (id, (p : Params.t ts)) ->
          match List.assoc_opt id states with
          | None -> states, acc
          | Some (tmr : States.t) ->
             (* collect event only if it is different
                  from the previous one or it is first *)
             let acc = match tmr.params.prev with
               | None -> (id, p) :: acc
               | Some prev ->
                  let eq = Params.equal in
                  if eq prev.data p.data then acc
                  else (id, p) :: acc in
             (* update previous field *)
             let params = Timer.(reset @@ set_prev tmr.params p) in
             let tmr = { tmr with params } in
             List.Assoc.set ~eq:(=) id tmr t.states, acc)
        (t.states, []) t.acc.params in
    (match acc with
     | [] -> ()
     | acc -> pe.params acc);
    { t with states }

  let handle_plps (pe : push_events) (t : t) : t =
    let states, acc =
      List.fold_left (fun (states, acc) (id, (p : Plp_list.t ts)) ->
          match List.assoc_opt id states with
          | None -> states, acc
          | Some (tmr : States.t) ->
             (* collect event only if it is different
                  from the previous one or it is first *)
             let acc = match tmr.plp_list.prev with
               | None -> (id, p) :: acc
               | Some prev ->
                  let eq = Plp_list.equal in
                  if eq prev.data p.data then acc
                  else (id, p) :: acc in
             (* update previous field *)
             let plp_list = Timer.(reset @@ set_prev tmr.plp_list p) in
             let tmr = { tmr with plp_list } in
             List.Assoc.set ~eq:(=) id tmr t.states, acc)
        (t.states, []) t.acc.plps in
    (match acc with
     | [] -> ()
     | acc -> pe.plp_list acc);
    { t with states }

  let handle_events (pe : push_events) (t : t) : t =
    handle_measurements pe t
    |> handle_params pe
    |> handle_plps pe
    |> fun t -> { t with acc = empty_acc }

  let make_pool (standards : (int * Device.standard) list)
        (states : (id * States.t) list) : pool =
    List.fold_left (fun acc (id, (tmr : States.t)) ->
        match List.assoc_opt id standards with
        | None -> acc
        | Some standard ->
           let is_t2 = Device.equal_standard standard T2 in
           let meas =
             if tmr.measures.timer = 0
             then Some (make_measure_probe id) else None in
           let params =
             if is_t2 && tmr.params.timer = 0
             then Some (make_params_probe id) else None in
           let plps =
             if is_t2 && tmr.plp_list.timer = 0
             then Some (make_plp_list_probe id) else None in
           meas ^:: params ^:: plps ^:: acc) [] states
    |> Pools.Pool.create

  let step t = { t with pool = Pools.Pool.step t.pool }

  let responsed t msgs = Pools.Pool.responsed t.pool msgs

  let empty t = Pools.Pool.empty t.pool

  let update_pool config t =
    let pool = make_pool config t.states in
    { t with pool }

  let last t = Pools.Pool.last t.pool

  let next t = { t with pool = Pools.Pool.next t.pool }

  let send t = Pools.Pool.send t.pool ()

  let make (config : (int * Device.standard) list) (receivers : int list) : t =
    let states = List.map (fun id -> id, States.empty) receivers in
    let pool = make_pool config states in
    { states; pool; acc = empty_acc }

end
