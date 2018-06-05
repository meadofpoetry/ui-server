open Containers
open Board_types
open Types
open Lwt.Infix
open Storage.Options
open Api.Handler
open Meta_board
open Meta_board.Msg
open Common

include Board_parser

let to_period x step_duration  = x * int_of_float (1. /. step_duration)

type device_events =
  { config : config React.event
  ; state  : Common.Topology.state React.signal
  ; input  : input React.signal
  ; status : status React.event
  ; reset  : reset_ts React.event
  ; errors : board_errors React.event
  }

type errors_events =
  { ts_errors   : Errors.TS.t list React.event
  ; t2mi_errors : Errors.T2MI.t list React.event
  }

type streams_events =
  { streams         : Common.Stream.t list React.signal
  ; ts_states       : Streams.TS.state list React.event
  ; ts_structures   : Streams.TS.structures React.signal
  ; ts_bitrates     : Streams.TS.structures React.signal
  ; t2mi_states     : Streams.T2MI.state list React.event
  ; t2mi_structures : Streams.T2MI.structures React.signal
  }

type jitter_events =
  { session : Jitter.session React.event
  ; jitter  : Jitter.measures React.event
  }

type events =
  { device  : device_events
  ; errors  : errors_events
  ; streams : streams_events
  ; jitter  : jitter_events
  }

type api =
  { get_devinfo     : unit            -> devinfo_opt Lwt.t
  ; set_input       : input           -> unit Lwt.t
  ; set_t2mi_mode   : t2mi_mode_opt   -> unit Lwt.t
  ; set_jitter_mode : jitter_mode_opt -> unit Lwt.t
  ; get_t2mi_seq    : int             -> Streams.T2MI.sequence Lwt.t
  ; get_section     : section_params  -> (Streams.TS.section,Streams.TS.section_error) Lwt_result.t
  ; reset           : unit            -> unit Lwt.t
  ; config          : unit            -> config Lwt.t
  }

module SM = struct

  let request_id = ref (-1)
  let jitter_ptr = ref (-1l)

  let get_id ()  = incr request_id; !request_id

  let wakeup_timeout (_,t) = t.pred `Timeout |> ignore

  type push_events =
    { devinfo        : devinfo_opt            -> unit
    ; state          : Topology.state         -> unit
    ; group          : group                  -> unit
    ; board_errors   : board_errors           -> unit
    ; structs        : Streams.TS.structures  -> unit
    ; bitrates       : Streams.TS.structures  -> unit
    ; t2mi_info      : Streams.T2MI.structure -> unit
    ; jitter         : Jitter.measures        -> unit
    ; jitter_session : Jitter.session         -> unit
    }

  module Events_handler : sig
    val handle          : event list -> group option -> group list * event list
    val update_versions : from:group -> group -> group
    val get_req_stack   : group -> group option -> probe_response probe_request list
    val to_ts_errors    : group -> Errors.TS.t list
    val to_ts_states    : group -> Streams.TS.state list
    val to_t2mi_errors  : group -> Errors.T2MI.t list
    val to_t2mi_states  : group -> Streams.T2MI.state list
  end = struct

    let update_versions ~(from:group) (t:group) : group =
      { t with status = { t.status with versions = from.status.versions }}

    let split_by l sep =
      let res,acc = List.fold_left (fun (res,acc) x ->
                        if Equal.poly x sep then (((List.rev acc) :: res),[]) else (res,x::acc))
                                   ([],[]) l in
      (List.rev res),(List.rev acc)

    (** Returns merged groups and unmerged events. Last group received is head of list **)
    let handle (events:event list) (prev_group:group option) =
      let groups,rest = split_by events `End_of_errors in
      let groups =
        List.filter (function
                     | `Status _ :: `Streams_event _ :: _ -> true
                     | _                                  -> false) groups
        |> List.fold_left (fun (acc:group list) (x:event list) ->
               let prev_status = (match acc with
                                  | [] -> Option.(prev_group >|= (fun x -> x.status))
                                  | x :: _ -> Some x.status) in
               match x with
               | `Status status :: `Streams_event streams :: events ->
                  ({ status = { status with streams = streams }
                   ; prev_status
                   ; events } : group) :: acc
               | _ -> assert false) []
      in groups,rest

    let get_req_stack ({ status; _ }:group) (prev_t:group option) =
      let bitrate_req    = Get_bitrates (get_id ()) in
      let jitter_req     = Get_jitter { request_id = get_id (); pointer = !jitter_ptr } in
      let board_errs_req = if status.errors then [ Get_board_errors (get_id ()) ] else [] in
      [ bitrate_req; jitter_req ]
      @ board_errs_req
      @ (match prev_t with
         | Some old -> (if old.status.versions.ts_ver_com <> status.versions.ts_ver_com
                        then [ Get_ts_structs (get_id ()) ] else [])
                       @ (List.map (fun id -> Get_t2mi_info { request_id = get_id (); stream_id = id })
                                   (List.foldi (fun acc i x ->
                                        if (x <> (List.nth old.status.versions.t2mi_ver_lst i))
                                           && List.mem ~eq:(=) i status.t2mi_sync
                                        then i :: acc
                                        else acc) [] status.versions.t2mi_ver_lst))
         | None -> [ Get_ts_structs (get_id ()) ]
                   @ (if List.is_empty status.t2mi_sync then []
                      else (List.map (fun id -> Get_t2mi_info { request_id = get_id (); stream_id = id })
                                     status.t2mi_sync)))

    let to_ts_errors (g:group) : Errors.TS.t list =
      let open Board_types.Errors.TS in
      let cmp = fun x y ->
        match Time.compare x.timestamp y.timestamp with
        | 0 -> Int32.compare x.packet y.packet
        | x -> x
      in g.events
         |> List.filter_map (function `Ts_errors x -> Some x | _ -> None)
         |> List.concat
         |> List.sort cmp

    let to_ts_states (g:group) : Streams.TS.state list =
      let open Board_types.Streams.TS in
      let eq = Stream.equal_id in
      let timestamp = g.status.status.timestamp in
      let ts_found =
        (match g.prev_status with
         | Some o -> List.filter (fun x -> not @@ List.mem ~eq x o.streams) g.status.streams
         | None   -> g.status.streams)
        |> List.map (fun x -> { stream = x; timestamp; present = true })
      in
      let ts_lost =
        (match g.prev_status with
         | Some o -> List.filter (fun x -> not @@ List.mem ~eq x g.status.streams) o.streams
         | None   -> [])
        |> List.map (fun x -> { stream = x; timestamp; present = false })
      in (ts_found @ ts_lost)

    let to_t2mi_errors (g:group) : Errors.T2MI.t list =
      let open Board_types.Errors.T2MI in
      List.filter_map (function `T2mi_errors x -> Some x | _ -> None) g.events
      |> List.concat
      |> List.sort (fun (x:t) y -> Time.compare x.timestamp y.timestamp)

    let to_t2mi_states (g:group) = match g.status.t2mi_mode with
      | Some m ->
         let eq        = Int.equal in
         let stream    = m.stream in
         let timestamp = g.status.status.timestamp in
         let conv = fun p s -> Streams.T2MI.({ stream; timestamp; stream_id = s; present = p }) in
         let lost = (match g.prev_status with
                     | Some o -> List.filter (fun x -> not @@ List.mem ~eq x o.t2mi_sync)
                                             g.status.t2mi_sync
                     | None   -> g.status.t2mi_sync)
                    |> List.map (conv true)
         in
         let found = (match g.prev_status with
                      | Some o -> List.filter (fun x -> not @@ List.mem ~eq x g.status.t2mi_sync)
                                              o.t2mi_sync
                      | None   -> [])
                     |> List.map (conv false)
         in
         lost @ found
      | None   -> []

  end

  module Pids = CCMap.Make(Int)

  let merge_service_and_bitrates (m:int Pids.t) (s:Streams.TS.service_info) =
    let open Streams.TS in
    let ecm = List.map (fun (ecm:ecm_info) -> { ecm with bitrate = Pids.get ecm.pid m }) s.ecm in
    let es  = List.map (fun (es:es_info)   -> { es with bitrate = Pids.get es.pid m }) s.es in
    let br  = List.fold_left (fun acc (x:es_info) ->
                  match acc,x.bitrate with
                  | None,Some x     -> Some x
                  | Some acc,Some x -> Some (acc + x)
                  | Some acc, None  -> Some acc
                  | None, None      -> None) None es in
    { s with bitrate = br; ecm; es }

  let merge_struct_and_bitrates (s:Streams.TS.structure) (b:Types.bitrate) =
    let open Streams.TS in
    let pids_m = List.fold_left (fun m ({pid;bitrate}:Types.pid_bitrate) ->
                     Pids.add pid bitrate m) Pids.empty b.pids in
    let pids = List.map (fun (pid:pid_info) -> { pid with bitrate = Pids.get pid.pid pids_m }) s.pids in
    let emm  = List.map (fun (emm:emm_info) -> { emm with bitrate = Pids.get emm.pid pids_m }) s.emm in
    let services = List.map (merge_service_and_bitrates pids_m) s.services in
    let update_table_common (c:table_info) = { c with bitrate = Pids.get c.pid pids_m } in
    let tables =
      List.map (function
                | PAT x     -> PAT { x with common = update_table_common x.common }
                | CAT x     -> CAT (update_table_common x)
                | PMT x     -> PMT { x with common = update_table_common x.common }
                | TSDT x    -> TSDT (update_table_common x)
                | NIT x     -> NIT { x with common = update_table_common x.common }
                | SDT x     -> SDT { x with common = update_table_common x.common }
                | BAT x     -> BAT { x with common = update_table_common x.common }
                | EIT x     -> EIT { x with common = update_table_common x.common }
                | TDT x     -> TDT (update_table_common x)
                | RST x     -> RST (update_table_common x)
                | ST  x     -> ST (update_table_common x)
                | TOT x     -> TOT (update_table_common x)
                | DIT x     -> DIT (update_table_common x)
                | SIT x     -> SIT (update_table_common x)
                | Unknown x -> Unknown (update_table_common x)) s.tables
    in { s with bitrate = Some b.ts_bitrate; pids; services; emm; tables; timestamp = b.timestamp }

  let merge_structs_and_bitrates (s:Streams.TS.structures) (b:Types.bitrates) =
    List.map (fun (s:Streams.TS.structure) ->
        match List.find_pred (fun (x:bitrate) -> Stream.equal_id x.stream s.stream) b with
        | Some x -> merge_struct_and_bitrates s x
        | None   -> s) s

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    (match msg with
     | Get_board_info       -> Get_board_info.to_cbuffer ()
     | Get_board_mode       -> Get_board_mode.to_cbuffer ()
     | Get_t2mi_frame_seq x -> Get_t2mi_frame_seq.to_cbuffer x
     | Get_section x        -> Get_section.to_cbuffer x)
    |> sender

  let send_event (type a) sender (msg : a probe_request) : unit Lwt.t =
    (match msg with
     | Get_board_errors id -> Get_board_errors.to_cbuffer id
     | Get_jitter req      -> Get_jitter.to_cbuffer req
     | Get_ts_structs req  -> Get_ts_structs.to_cbuffer req
     | Get_bitrates req    -> Get_bitrates.to_cbuffer req
     | Get_t2mi_info req   -> Get_t2mi_info.to_cbuffer req)
    |> sender

  let send_instant (type a) sender (msg : a instant_request) : unit Lwt.t =
    (match msg with
     | Reset             -> to_complex_req ~msg_code:0x0111 ~body:(Cbuffer.create 0) ()
     | Set_board_mode x  ->
        let t2mi = Option.get_or ~default:{ enabled        = false
                                          ; pid            = 0
                                          ; t2mi_stream_id = 0
                                          ; stream         = Single
                                          }
                                 x.t2mi in
        let body = Cbuffer.create sizeof_board_mode in
        let () = input_to_int x.input
                 |> (lor) (if t2mi.enabled then 4 else 0)
                 |> (lor) 8 (* disable board storage by default *)
                 |> set_board_mode_mode body in
        let () = set_board_mode_t2mi_pid body t2mi.pid in
        let () = set_board_mode_t2mi_stream_id body (Stream.id_to_int32 t2mi.stream) in
        to_simple_req ~msg_code:0x0082 ~body ()
     | Set_jitter_mode x ->
        let req  = match x with
          | Some x -> x
          | None   -> { pid = 0x1fff; stream = Single }
        in
        let body = Cbuffer.create sizeof_req_set_jitter_mode in
        let () = set_req_set_jitter_mode_stream_id body (Stream.id_to_int32 req.stream) in
        let () = set_req_set_jitter_mode_pid body req.pid in
        to_complex_req ~msg_code:0x0112 ~body ())
    |> sender

  let enqueue (type a) state msgs sender (msg : a request) timeout exn : a Lwt.t =
    (* no instant msgs *)
    match React.S.value state with
    | `Fine ->
       let t,w = Lwt.wait () in
       let pred = function
         | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
         | l        -> Option.( is_response msg l >|= Lwt.wakeup w ) in
       let send = fun () -> send_msg sender msg in
       msgs := Await_queue.append !msgs { send; pred; timeout; exn };
       t
    | _ -> Lwt.fail (Failure "board is not responding")

  let enqueue_instant (type a) state msgs sender (storage:config storage) (msg : a instant_request) : unit Lwt.t =
    match React.S.value state with
    | `Fine ->
       let t,w = Lwt.wait () in
       let send = fun () -> (send_instant sender msg) >>= (fun x -> Lwt.return @@ Lwt.wakeup w x) in
       let pred = fun _  -> None in
       let conf = storage#get in
       let _    = match msg with
         | Set_board_mode  mode -> storage#store { conf with t2mi_mode = mode.t2mi; input = mode.input }
         | Set_jitter_mode x    -> storage#store { conf with jitter_mode = x }
         | Reset                -> ()
       in
       msgs := Queue.append !msgs { send; pred; timeout = 0; exn = None };
       t
    | _ -> Lwt.fail (Failure "board is not responding")

  let handle_probes_acc (pe:push_events) (acc:probe_response list) =
    let s  = List.find_map (function Struct s  -> Some s | _ -> None) acc in
    let b  = List.find_map (function Bitrate b -> Some b | _ -> None) acc in
    let () = match s,b with
      | Some s,Some b -> pe.bitrates @@ merge_structs_and_bitrates s b
      | _             -> ()
    in List.iter (function
                  | Board_errors x -> pe.board_errors x
                  | Struct x       -> pe.structs x
                  | T2mi_info x    -> pe.t2mi_info x
                  | Jitter x       -> jitter_ptr := x.next_ptr; pe.jitter x.measures
                  | Bitrate _      -> ())
                 acc

  let sm_step msgs imsgs sender (storage : config storage) step_duration push =
    let period         = to_period 5 step_duration in

    let handle_msgs rsps =
      if Await_queue.has_pending !msgs
      then (msgs := fst @@ Await_queue.responsed !msgs rsps;
            let new_msgs,tout = Await_queue.step !msgs in
            msgs := new_msgs;
            (match tout with
             | [] -> ()
             | l  -> List.iter (fun x -> x.pred `Timeout |> ignore) l));
      if not @@ Await_queue.empty !msgs
      then msgs := fst @@ Await_queue.send !msgs () in

    let rec first_step () =
      Await_queue.iter !msgs wakeup_timeout;
      msgs := Await_queue.create [];
      imsgs := Queue.create [];
      push.state `No_response;
      send_msg sender Get_board_info |> ignore;
      `Continue (step_detect period None)

    and step_detect p acc recvd =
      let _, _, rsps, _, acc = deserialize [] (Meta_board.concat_acc acc recvd) in
      match List.find_map (is_response Get_board_info) rsps with
      | Some info -> push.state `Init;
                     push.devinfo (Some info);
                     let config = storage#get in
                     send_instant sender (Set_board_mode { t2mi  = config.t2mi_mode
                                                         ; input = config.input }) |> ignore;
                     send_instant sender (Set_jitter_mode config.jitter_mode)
                     |> ignore;
                     (* send_instant sender Reset |> ignore; *)
                     `Continue (step_ok_idle period None [] [] None)
      | None      -> if p < 0 then first_step ()
                     else `Continue (step_detect (pred p) acc)

    and step_ok_idle p prev_group prev_events parts acc recvd =
      let events,_,rsps,parts,acc = deserialize parts (Meta_board.concat_acc acc recvd) in
      if Option.is_none @@ List.find_map (is_response Get_board_info) rsps
      then
        let events = prev_events @ events in
        let ()     = handle_msgs rsps in
        let _      = Queue.send !imsgs () (* FIXME *) in
        let ()     = imsgs := Queue.next !imsgs in
        (match Events_handler.handle events prev_group with
         | [],events ->
            if p < 0 then first_step ()
            else `Continue (step_ok_idle (pred p) prev_group events parts acc)
         | (hd::_) as groups,events ->
            push.state `Fine;
            List.iter (fun x -> push.group x) @@ List.rev groups;
            let pool =
              List.map (fun req -> { send    = (fun () -> send_event sender req)
                                   ; pred    = (is_probe_response req)
                                   ; timeout = period
                                   ; exn     = None })
                       (Events_handler.get_req_stack hd prev_group)
              |> Pool.create
            in
            step_ok_probes_send [] pool hd hd events parts acc)
      else first_step ()

    and step_ok_probes_send probes_acc pool prev_idle_gp gp events parts acc =
      if Pool.empty pool
      then `Continue (step_ok_idle period (Some gp) events parts acc)
      else (Pool.send pool () |> ignore;
            `Continue (step_ok_probes_wait probes_acc pool period prev_idle_gp gp events parts acc))

    and step_ok_probes_wait probes_acc pool p idle_gp gp events parts acc recvd =
      let ev,probes,rsps,parts,acc = deserialize parts (Meta_board.concat_acc acc recvd) in
      let () = handle_msgs rsps in
      let gp,ev = match Events_handler.handle (events @ ev) (Some gp) with
        | [ ],e          -> gp,e
        | (hd::_) as l,e -> List.iter (fun x -> push.group x) @@ List.rev l; hd,e
      in
      try
        (match Pool.responsed pool probes with
         | None   -> let pool = Pool.step pool in
                     let p    = pred p in
                     `Continue (step_ok_probes_wait probes_acc pool p idle_gp gp ev parts acc)
         | Some x ->
            let probes_acc = x :: probes_acc in
            match Pool.last pool with
            | true  -> let () = handle_probes_acc push probes_acc in
                       let gp = Some (Events_handler.update_versions ~from:idle_gp gp) in
                       `Continue (step_ok_idle period gp ev parts acc)
            | false -> step_ok_probes_send probes_acc (Pool.next pool) idle_gp gp ev parts acc)
      with
      | Timeout -> first_step ()

    in first_step ()

  let to_t2mi_info_s (i:Streams.T2MI.structure React.event) : Streams.T2MI.structures React.signal =
    React.E.fold (fun acc x ->
        List.add_nodup ~eq:(fun (x:Streams.T2MI.structure) y -> x.stream_id = y.stream_id) x acc) [] i
    |> React.S.hold ~eq:(fun _ _ -> false) []

  let to_input_e (group:group React.event) : input React.event =
    React.E.changes ~eq:equal_input @@ React.E.map (fun (x:group) -> x.status.input) group

  let to_status_e (group:group React.event) : status React.event =
    React.E.changes ~eq:equal_status @@ React.E.map (fun (x:group) -> x.status.status) group

  let to_reset_e (group:group React.event) : reset_ts React.event =
    React.E.map (fun (x:group) : reset_ts -> { timestamp = x.status.status.timestamp }) group

  let to_ts_errors_e (group:group React.event) : Errors.TS.t list React.event =
    React.E.map (fun g -> Events_handler.to_ts_errors g) group

  let to_ts_states_e (group:group React.event) : Streams.TS.state list React.event =
    React.E.map (fun g -> Events_handler.to_ts_states g) group

  let to_t2mi_errors_e (group:group React.event) : Errors.T2MI.t list React.event =
    React.E.map (fun g -> Events_handler.to_t2mi_errors g) group

  let to_t2mi_states_e (group:group React.event) : Streams.T2MI.state list React.event =
    React.E.map (fun g -> Events_handler.to_t2mi_states g) group

  let to_raw_streams_s (group:group React.event) : Stream.stream list React.signal =
    let conv : input -> Stream.id -> Stream.stream = fun i x ->
      { id          = `Ts x
      ; description = Some ""
      ; source      = (match x with (* FIXME *)
                       | T2mi_plp _ -> Stream Single
                       | _          -> Port (match i with SPI -> 0 | ASI -> 1))
      }
    in
    React.E.map (fun (g:group) -> List.map (fun x -> conv g.status.input x) g.status.streams) group
    |> React.S.hold ~eq:(Equal.list Stream.equal_stream) []

  let to_raw_input_streams_s (streams:Stream.stream list React.signal) : Stream.stream list React.signal =
    React.S.map (List.filter (fun (x:Stream.stream) -> match x.source with Port _ -> true | _ -> false)) streams

  let create sender (storage : config storage) step streams_conv =
    let msgs   = ref (Await_queue.create []) in
    let imsgs  = ref (Queue.create []) in
    let s_state,state_push = React.S.create `No_response in
    let e_group,group_push = React.E.create () in
    let e_cfg,cfg_push     = React.E.create () in
    let s_devi,devi_push   = React.S.create None in
    let e_be,be_push       = React.E.create () in
    let s_ts,ts_push       = React.S.create ~eq:(fun _ _ -> false) [] in
    let s_br,br_push       = React.S.create ~eq:(fun _ _ -> false) [] in
    let e_t2mi,t2mi_push   = React.E.create () in
    let e_pcr,pcr_push     = React.E.create () in
    let e_pcr_s,pcr_s_push = React.E.create () in
    let s_t2mi             = to_t2mi_info_s e_t2mi in
    let s_raw_streams      = to_raw_streams_s e_group in
    let (events : events) =
      { device  = { config = React.E.changes ~eq:equal_config e_cfg
                  ; state  = s_state
                  ; status = to_status_e e_group
                  ; input  = React.S.hold ~eq:equal_input storage#get.input @@ to_input_e e_group
                  ; reset  = to_reset_e e_group
                  ; errors = e_be
                  }
      ; errors  = { ts_errors   = to_ts_errors_e e_group
                  ; t2mi_errors = to_t2mi_errors_e e_group
                  }
      ; streams = { streams         = streams_conv s_raw_streams
                  ; ts_states       = to_ts_states_e e_group
                  ; ts_structures   = s_ts
                  ; ts_bitrates     = s_br
                  ; t2mi_states     = to_t2mi_states_e e_group
                  ; t2mi_structures = s_t2mi
                  }
      ; jitter  = { session = React.E.changes ~eq:Jitter.equal_session e_pcr_s
                  ; jitter  = e_pcr
                  }
      }
    in
    let push_events =
      { devinfo        = devi_push
      ; state          = state_push
      ; group          = group_push
      ; board_errors   = be_push
      ; structs        = ts_push
      ; bitrates       = br_push
      ; t2mi_info      = t2mi_push
      ; jitter         = pcr_push
      ; jitter_session = pcr_s_push
      }
    in
    let api =
      { set_input       = (fun i -> let m = { t2mi = storage#get.t2mi_mode; input = i } in
                                    enqueue_instant s_state imsgs sender storage (Set_board_mode m)
                                    >>= (fun () -> cfg_push storage#get; Lwt.return_unit))
      ; set_t2mi_mode   = (fun x -> let m = { input = storage#get.input; t2mi = x } in
                                    enqueue_instant s_state imsgs sender storage (Set_board_mode m)
                                    >>= (fun () -> cfg_push storage#get; Lwt.return_unit))
      ; set_jitter_mode = (fun m -> enqueue_instant s_state imsgs sender storage (Set_jitter_mode m)
                                    >>= (fun () -> cfg_push storage#get; Lwt.return_unit))
      ; get_devinfo     = (fun () -> Lwt.return @@ React.S.value s_devi)
      ; reset           = (fun () -> enqueue_instant s_state imsgs sender storage Reset)
      ; get_section     = (fun r -> enqueue s_state msgs sender
                                            (Get_section { request_id = get_id (); params = r })
                                            (to_period 125 step) None)
      ; get_t2mi_seq    = (fun s -> enqueue s_state msgs sender
                                            (Get_t2mi_frame_seq { request_id = get_id (); seconds = s })
                                            (to_period (s + 10) step)
                                            None)
      ; config          = (fun () -> Lwt.return storage#get)
      }
    in events,api,(sm_step msgs imsgs sender storage step push_events)

end
