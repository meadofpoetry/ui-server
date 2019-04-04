open Board_niitv_dvb_types
open Application_types
open Boards.Util

let ( ^:: ) x l = match x with
  | None -> l
  | Some x -> x :: l

(* TODO remove from here. *)
type push_events =
  { measure : (int * Measure.t ts) list -> unit
  ; params : (int * Params.t ts) list -> unit
  ; plp_list : (int * Plp_list.t ts) list -> unit
  ; state : Topology.state -> unit
  ; devinfo : Device.info option -> unit
  }

open Boards

module States = struct

  type 'a state =
    { prev : 'a option
    ; timeout : float
    ; timer : unit Lwt.t
    }

  type t =
    { measures : Measure.t ts state
    ; plp_list : Plp_list.t ts state
    ; params : Params.t ts state
    }

  let reset (state : 'a state) : 'a state =
    { state with timer = Lwt_unix.sleep state.timeout }

  let set_prev (prev : 'a) (state : 'a state) : 'a state =
    { state with prev = Some prev }

  let is_ready (state : 'a state) : bool =
    match Lwt.state state.timer with
    | Lwt.Return () -> true
    | Lwt.Fail exn -> raise exn
    | Lwt.Sleep -> false

  let empty : t =
    { measures =
        { prev = None
        ; timeout = 1.
        ; timer = Lwt.return ()
        }
    ; plp_list =
        { prev = None
        ; timeout = 5.
        ; timer = Lwt.return ()
        }
    ; params =
        { prev = None
        ; timeout = 5.
        ; timer = Lwt.return ()
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
  ; timeout : float
  ; send : Parser.event Parser.event_request -> unit Lwt.t
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

let make_req timeout send req =
  let ( >>= ) = Lwt.bind in
  let pred w : 'a Pools.resolver = function
    | `Tm -> ()
    | `Msgs msgs ->
       match Util.List.find_map (Parser.is_event req) msgs with
       | None -> ()
       | Some x -> Lwt.wakeup w (`V x) in
  let send () =
    let t, w = Lwt.wait () in
    Lwt.catch (fun () ->
        send req
        >>= fun () -> Lwt_unix.with_timeout timeout (fun () -> t))
      (function
       | Lwt_unix.Timeout -> Lwt.return `Tm
       | exn -> Lwt.fail exn),
    pred w in
  send

let cons_event (t : t) (event : Parser.event) : t =
  let stamp : 'a. 'a -> 'a ts = fun data ->
    { timestamp = Ptime_clock.now (); data } in
  let set id v lst = List.Assoc.set ~eq:(=) id (stamp v) lst in
  let acc = match event with
    | Measures (id, m) -> { t.acc with measures = set id m t.acc.measures }
    | Params (id, p) -> { t.acc with params = set id p t.acc.params }
    | Plp_list (id, p) -> { t.acc with plps = set id p t.acc.plps } in
  { t with acc }

let handle_measurements (pe : push_events) (t : t) : t =
  (* Push measures in any case *)
  pe.measure t.acc.measures;
  let states =
    List.fold_left (fun states (id, m) ->
        match List.assoc_opt id states with
        | None -> states
        | Some (tmr : States.t) ->
           let measures = States.(reset @@ set_prev m tmr.measures) in
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
           let params = States.(reset @@ set_prev p tmr.params) in
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
           let plp_list = States.(reset @@ set_prev p tmr.plp_list) in
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

let make_pool (timeout : float)
      (send : Parser.event Parser.event_request -> unit Lwt.t)
      (standards : (int * Device.standard) list)
      (states : (id * States.t) list) : pool =
  List.fold_left (fun acc (id, (tmr : States.t)) ->
      match List.assoc_opt id standards with
      | None -> acc
      | Some standard ->
         let is_t2 = Device.equal_standard standard T2 in
         let meas =
           if States.is_ready tmr.measures
           then Some (make_req timeout send (Parser.Get_measure id)) else None in
         let params =
           if is_t2 && States.is_ready tmr.params
           then Some (make_req timeout send (Parser.Get_params id)) else None in
         let plps =
           if is_t2 && States.is_ready tmr.plp_list
           then Some (make_req timeout send (Parser.Get_plp_list id)) else None in
         meas ^:: params ^:: plps ^:: acc) [] states
  |> Pools.Pool.create

let is_empty t = Pools.Pool.is_empty t.pool

let update config t =
  let pool = make_pool t.timeout t.send config t.states in
  { t with pool }

let is_last t = Pools.Pool.is_last t.pool

let send t = { t with pool = Pools.Pool.send t.pool }

let make (timeout : float)
      (send : Parser.event Parser.event_request -> unit Lwt.t)
      (config : (int * Device.standard) list)
      (receivers : int list) : t =
  let states = List.map (fun id -> id, States.empty) receivers in
  let pool = make_pool timeout send config states in
  { states; pool; acc = empty_acc; timeout; send }

let apply (t : t) m =
  { t with pool = Pools.Pool.apply t.pool m }

let _match (t : t) ~resolved ~timeout ~pending ~not_sent =
  Pools.Pool._match t.pool
    ~resolved:(fun pool x -> resolved (cons_event { t with pool } x) x)
    ~pending:(fun pool -> pending { t with pool })
    ~timeout:(fun pool -> timeout { t with pool })
    ~not_sent:(fun pool -> not_sent { t with pool })
