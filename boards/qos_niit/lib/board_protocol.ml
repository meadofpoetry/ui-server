open Board_types
open Lwt.Infix
open Storage.Options
open Api.Handler
open Meta_board
open Meta_board.Msg

include Board_parser

let io x = Lwt_io.printf "%s\n" x |> ignore

let to_period x step_duration  = x * int_of_float (1. /. step_duration)

module SM = struct

  let request_id = ref (-1)
  let jitter_ptr = ref (-1l)

  exception T2mi_seq_timeout
  exception Streams_timeout

  let get_id ()  = incr request_id; !request_id

  let wakeup_timeout (_,t) = t.pred `Timeout |> ignore

  type events = { status       : user_status React.event
                ; ts_found     : Common.Stream.t React.event
                ; ts_lost      : Common.Stream.t React.event
                ; ts_errors    : ts_errors React.event
                ; t2mi_found   : int React.event
                ; t2mi_lost    : int React.event
                ; t2mi_errors  : t2mi_errors React.event
                ; board_errors : board_errors React.event
                ; bitrate      : bitrate list React.event
                ; structs      : ts_struct list React.event
                ; t2mi_info    : t2mi_info React.event
                ; jitter       : jitter React.event
                }

  type push_events = { status       : user_status     -> unit
                     ; ts_found     : Common.Stream.t -> unit
                     ; ts_lost      : Common.Stream.t -> unit
                     ; ts_errors    : ts_errors       -> unit
                     ; t2mi_found   : int             -> unit
                     ; t2mi_lost    : int             -> unit
                     ; t2mi_errors  : t2mi_errors     -> unit
                     ; board_errors : board_errors    -> unit
                     ; bitrate      : bitrate list    -> unit
                     ; structs      : ts_struct list  -> unit
                     ; t2mi_info    : t2mi_info       -> unit
                     ; jitter       : jitter          -> unit
                     }

  module Events_handler : sig
    type event = [ `Status      of Board_types.status
                 | `Streams_event of streams
                 | `T2mi_errors of Cbuffer.t
                 | `Ts_errors   of Cbuffer.t ]
    type t
    val partition        : event list -> t option -> event list * t list
    val insert_events    : t -> event list -> t
    val insert_versions  : t -> t -> t
    val get_req_stack    : t -> t option -> event_response event_request list
    val push             : t -> push_events -> unit
  end = struct
    type event = [ `Status of Board_types.status
                 | `Streams_event of streams
                 | `T2mi_errors of Cbuffer.t
                 | `Ts_errors of Cbuffer.t ]

    type t =
      { status       : status
      ; prev_status  : status option
      ; events       : event list
      }

    let group_ts_errs l   = CCList.filter_map (function
                                               | `Ts_errors x -> try_parse of_ts_errors x
                                               | _            -> None) l
                            |> CCList.group_succ ~eq:(fun (x:ts_errors) y -> x.stream_id = y.stream_id)
                            |> CCList.map (fun l -> CCList.fold_left (fun (acc : ts_errors) (x : ts_errors) ->
                                                        { acc with errors = (acc.errors @ x.errors) })
                                                                     (CCList.hd l) l)

    let sort_ts_errs l    = CCList.map (fun (x : ts_errors) ->
                                { x with errors = CCList.sort (fun (x : ts_error) (y : ts_error) ->
                                                      Int32.compare x.packet y.packet) x.errors }) l

    let group_t2mi_errs l = CCList.filter_map (function
                                               | `T2mi_errors x -> try_parse of_t2mi_errors x
                                               | _ -> None) l
                            |> CCList.group_succ ~eq:(fun (x : t2mi_errors) y -> x.stream_id = y.stream_id)
                            |> CCList.map (fun l -> CCList.fold_left (fun (acc : t2mi_errors) (x : t2mi_errors) ->
                                                        { acc with errors = (acc.errors @ x.errors) })
                                                                     (CCList.hd l) l)

    let insert_events t events = { t with events = t.events @ events }

    let insert_versions t old_t = { t with status = { t.status with versions = old_t.status.versions }}

    let partition events prev_group =
      match CCList.find_idx (function | `Status _ -> true | _ -> false) events with
      | Some (idx,_) -> let ev,other = CCList.take_drop idx events in
                        let rec f = (fun acc (l : event list) ->
                            let events,groups = acc in
                            match l with
                            | [] -> acc
                            | (`Status status :: `Streams_event streams :: tl) ->
                               let groups,prev_status = (match groups with
                                                         | []      -> let open CCOpt in
                                                                      groups,prev_group >|= (fun x -> x.status)
                                                         | x :: tl -> let prev_group = insert_events x events in
                                                                      prev_group :: tl,Some prev_group.status) in
                               f ([], { status      = { status with streams = streams}
                                      ; prev_status = prev_status
                                      ; events      = [] } :: groups) tl
                            | `Status _ :: _ :: _ -> failwith "got status without streams"
                            | x :: tl -> f (x::events,groups) tl) in
                        let events,groups = f ([],[]) other in
                        let groups = (match groups with
                                      | [] -> []
                                      | x :: tl -> (insert_events x events) :: tl) in
                        ev,CCList.rev groups
      | None       -> events,[]

    let get_req_stack { status; _ } prev_t =
      let bitrate_req    = Get_bitrates (get_id ()) in
      let jitter_req     = Get_jitter { request_id = get_id (); pointer = !jitter_ptr } in
      let board_errs_req = if status.errors then [ Get_board_errors (get_id ()) ] else [] in
      [ bitrate_req; jitter_req ]
      @ board_errs_req
      @ (match prev_t with
         | Some old -> (if old.status.versions.ts_ver_com <> status.versions.ts_ver_com
                        then [ Get_ts_structs (get_id ()) ] else [])
                       @ (List.map (fun id -> Get_t2mi_info { request_id = get_id (); stream_id = id })
                                   (CCList.foldi (fun acc i x ->
                                        if (x <> (CCList.nth old.status.versions.t2mi_ver_lst i))
                                           && List.mem i status.t2mi_sync
                                        then i :: acc
                                        else acc) [] status.versions.t2mi_ver_lst))
         | None -> [ Get_ts_structs (get_id ()) ]
                   @ (if CCList.is_empty status.t2mi_sync then []
                      else (List.map (fun id -> Get_t2mi_info { request_id = get_id (); stream_id = id })
                                     status.t2mi_sync)))

    let push t (pe : push_events) =
      let ts_found   = (match t.prev_status with
                        | Some o -> List.filter (fun x -> not @@ List.mem x o.streams) t.status.streams
                        | None   -> t.status.streams) in
      let ts_lost    = (match t.prev_status with
                        | Some o -> List.filter (fun x -> not @@ List.mem x t.status.streams) o.streams
                        | None   -> []) in
      let t2mi_found = (match t.prev_status with
                        | Some o -> List.filter (fun x -> not @@ List.mem x o.t2mi_sync) t.status.t2mi_sync
                        | None   -> t.status.t2mi_sync) in
      let t2mi_lost  = (match t.prev_status with
                        | Some o -> List.filter (fun x -> not @@ List.mem x t.status.t2mi_sync) o.t2mi_sync
                        | None   -> []) in
      pe.status t.status.status;
      List.iter pe.ts_found ts_found;
      List.iter pe.ts_errors @@ sort_ts_errs @@ group_ts_errs t.events;
      List.iter pe.t2mi_found t2mi_found;
      List.iter pe.t2mi_errors @@ group_t2mi_errs t.events;
      List.iter pe.t2mi_lost t2mi_lost;
      List.iter pe.ts_lost ts_lost

  end

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    (match msg with
     | Get_board_info       -> to_common_header ~msg_code:0x0080 ()
     | Get_board_mode       -> to_common_header ~msg_code:0x0081 ()
     | Get_t2mi_frame_seq x -> let body = Cbuffer.create sizeof_req_get_t2mi_frame_seq in
                                          let ()   = set_req_get_t2mi_frame_seq_time body x.seconds in
                                          to_complex_req ~request_id:x.request_id ~msg_code:0x0306 ~body ()
     | Get_section (id,req) -> to_complex_req ~request_id:id ~msg_code:0x0302 ~body:(to_req_get_section req) ())
    |> sender

  let send_event (type a) sender (msg : a event_request) : unit Lwt.t =
    (match msg with
     | Get_board_errors id -> (* io "sent board errors"; *)
                              to_complex_req ~request_id:id
                                             ~msg_code:0x0110
                                             ~body:(Cbuffer.create 0) ()
     | Get_jitter req      -> (* io "sent jitter"; *)
                              let body = Cbuffer.create sizeof_req_get_jitter in
                              let ()   = set_req_get_jitter_ptr body req.pointer in
                              to_complex_req ~request_id:req.request_id
                                             ~msg_code:0x0307
                                             ~body ()
     | Get_ts_structs req  -> (* io "sent ts structs"; *)
                              let stream = (Common.Stream.of_int32 0xFFFFFFFFl) in
                              let body = Cbuffer.create sizeof_req_get_ts_struct in
                              let ()   = set_req_get_ts_struct_stream_id body @@ Common.Stream.to_int32 stream in
                              to_complex_req ~request_id:req
                                             ~msg_code:0x0309
                                             ~body ()
     | Get_bitrates req    -> (* io "sent bitrates"; *)
                              to_complex_req ~request_id:req ~msg_code:0x030A ~body:(Cbuffer.create 0) ()
     | Get_t2mi_info req   -> (* io "sent t2mi info"; *)
                              let body = Cbuffer.create sizeof_req_get_t2mi_info in
                              let ()   = set_req_get_t2mi_info_stream_id body req.stream_id in
                              to_complex_req ~request_id:req.request_id
                                             ~msg_code:0x030B
                                             ~body ())
    |> sender

  let send_instant (type a) sender (msg : a instant_request) : unit Lwt.t =
    (match msg with
     | Set_board_mode x  -> let t2mi = CCOpt.get_or ~default:{ enabled = false;
                                                               pid = 0;
                                                               stream_id = Single
                                                             } x.t2mi in
                            let body = Cbuffer.create sizeof_board_mode in
                            let ()   = input_to_int x.input
                                       |> (lor) (if t2mi.enabled then 4 else 0)
                                       |> (lor) 8 (* disable board storage by default *)
                                       |> set_board_mode_mode body in
                            let ()   = set_board_mode_t2mi_pid body t2mi.pid in
                            let ()   = set_board_mode_t2mi_stream_id body (Common.Stream.to_int32 t2mi.stream_id) in
                            to_simple_req ~msg_code:0x0082 ~body ()
     | Reset             -> to_complex_req ~msg_code:0x0111 ~body:(Cbuffer.create 0) ()
     | Set_jitter_mode x -> let body = Cbuffer.create sizeof_req_set_jitter_mode in
                            let ()   = set_req_set_jitter_mode_stream_id body (Common.Stream.to_int32 x.stream_id) in
                            let ()   = set_req_set_jitter_mode_pid body x.pid in
                            to_complex_req ~msg_code:0x0112 ~body ())
    |> sender

  let enqueue (type a) msgs sender (msg : a request) timeout exn : a Lwt.t =
    (* no instant msgs *)
    let t,w = Lwt.wait () in
    let pred = function
      | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
      | l        -> CCOpt.( is_response msg l >|= Lwt.wakeup w ) in
    let send = fun () -> send_msg sender msg in
    msgs := Await_queue.append !msgs { send; pred; timeout; exn };
    t

  let enqueue_instant (type a) msgs sender (msg : a instant_request) : unit Lwt.t =
    let t,w = Lwt.wait () in
    let send = fun () -> (send_instant sender msg) >>= (fun x -> Lwt.return @@ Lwt.wakeup w x) in
    let pred = fun _  -> None in
    msgs := Queue.append !msgs { send; pred; timeout = 0; exn = None };
    t

  let push_event_response pe = function
    | Board_errors x -> (* io "got board errors"; *)
                        pe.board_errors x
    | Bitrate x      -> (* io "got bitrate"; *)
                        pe.bitrate x
    | Struct  x      -> (* io "got struct"; *)
                        pe.structs x
    | T2mi_info x    -> (* io "got t2mi info"; *)
                        pe.t2mi_info x
    | Jitter x       -> (* io "got jitter"; *)
                        pe.jitter x

  let step msgs imsgs sender (storage : config storage) step_duration push_state push_events =
    let period         = to_period 5 step_duration in
    (* let section_period = to_period 120 step_duration in *)

    let handle_msgs rsps =
      if Await_queue.has_pending !msgs
      then (msgs := fst @@ Await_queue.responsed !msgs rsps;
            let new_msgs,tout = Await_queue.step !msgs in
            msgs := new_msgs;
            (match tout with
             | [] -> ()
             | l  -> CCList.iter (fun x -> x.pred `Timeout |> ignore) l));
      if not @@ Await_queue.empty !msgs
      then msgs := fst @@ Await_queue.send !msgs () in

    let rec first_step () =
      Await_queue.iter !msgs wakeup_timeout;
      msgs := Await_queue.create [];
      imsgs := Queue.create [];
      push_state `No_response;
      send_msg sender Get_board_info |> ignore;
      `Continue (step_detect period None)

    and step_detect p acc recvd =
      let _, _, rsps, _, acc = deserialize [] (Meta_board.concat_acc acc recvd) in
      if CCOpt.is_some @@ CCList.find_map (is_response Get_board_info) rsps
      then (push_state `Init;
            let config = storage#get in
            send_instant sender (Set_board_mode config.mode) |> ignore;
            send_instant sender (Set_jitter_mode config.jitter_mode)
            |> ignore;
            (* send_instant sender Reset |> ignore; *)
            `Continue (step_normal_idle period None [] [] None))
      else (if p < 0 then first_step ()
            else `Continue (step_detect (pred p) acc))

    and step_normal_idle p prev_group prev_events parts acc recvd =
      let events,_,rsps,parts,acc = deserialize parts (Meta_board.concat_acc acc recvd) in
      if CCOpt.is_none @@ CCList.find_map (is_response Get_board_info) rsps
      then
        let events = prev_events @ events in
        handle_msgs rsps;
        (match Events_handler.partition events prev_group with
         | events,[]  -> if p < 0
                         then first_step ()
                         else `Continue (step_normal_idle (pred p) prev_group events parts acc)
         | prev_group_events,groups ->
            push_state `Fine;
            io @@ Printf.sprintf "Got %d statuses in idle!" @@ CCList.length groups;
            (match prev_group with
             | Some gp -> Events_handler.push (Events_handler.insert_events gp prev_group_events) push_events
             | None    -> ());
            let other = CCList.take (CCList.length groups - 1) groups in
            let last  = CCOpt.get_exn @@ CCList.last_opt groups in
            CCList.iter (fun x -> Events_handler.push x push_events) other;
            let pool = Pool.create @@ List.map (fun req -> { send    = (fun () -> send_event sender req)
                                                           ; pred    = (is_event req)
                                                           ; timeout = period
                                                           ; exn     = None
                                               })
                                               (Events_handler.get_req_stack last prev_group) in
            step_normal_probes_send pool last last events parts acc)
      else first_step ()

    and step_normal_probes_send pool prev_idle_gp gp events parts acc =
      if Pool.empty pool
      then `Continue (step_normal_idle period (Some gp) events parts acc)
      else (Pool.send pool () |> ignore;
            `Continue (step_normal_probes_wait pool period prev_idle_gp gp events parts acc))

    and step_normal_probes_wait pool p prev_idle_gp gp prev_events parts acc recvd =
      let events,ev_rsps,rsps,parts,acc = deserialize parts (Meta_board.concat_acc acc recvd) in
      let events    = prev_events @ events in
      handle_msgs rsps;
      let events,gp = (match Events_handler.partition events (Some gp) with
                       | events, []            -> events,gp
                       | prev_gp_events,groups ->
                          Events_handler.push (Events_handler.insert_events gp prev_gp_events)
                                              push_events;
                          CCList.iter (fun x -> Events_handler.push x push_events) groups;
                          [],CCOpt.get_exn @@ CCList.last_opt groups) in
      try
        (match Pool.responsed pool ev_rsps with
         | None   -> let pool = Pool.step pool in
                     `Continue (step_normal_probes_wait pool (pred p) prev_idle_gp gp events parts acc)
         | Some x -> push_event_response push_events x;
                     (match x with
                      | Jitter x -> jitter_ptr := x.next_ptr;
                      | _        -> ());
                     let new_pool = Pool.next pool in
                     if Pool.last pool
                     then `Continue (step_normal_idle period
                                                      (Some (Events_handler.insert_versions gp prev_idle_gp))
                                                      prev_events
                                                      parts
                                                      acc)
                     else step_normal_probes_send new_pool prev_idle_gp gp events parts acc)
      with
      | Timeout -> io "\n!!! exit by timeout !!!\n"; first_step ()

    in first_step ()

  let create sender (storage : config storage) push_state step_duration =
    let msgs   = ref (Await_queue.create []) in
    let imsgs  = ref (Queue.create []) in
    let status,status_push             = React.E.create () in
    let ts_found,ts_found_push         = React.E.create () in
    let ts_lost,ts_lost_push           = React.E.create () in
    let ts_errors,ts_errors_push       = React.E.create () in
    let t2mi_found,t2mi_found_push     = React.E.create () in
    let t2mi_lost,t2mi_lost_push       = React.E.create () in
    let t2mi_errors,t2mi_errors_push   = React.E.create () in
    let board_errors,board_errors_push = React.E.create () in
    let bitrate,bitrate_push           = React.E.create () in
    let structs,structs_push           = React.E.create () in
    let t2mi_info,t2mi_info_push       = React.E.create () in
    let jitter,jitter_push             = React.E.create () in
    let (events : events)              = { status
                                         ; ts_found
                                         ; ts_lost
                                         ; ts_errors
                                         ; t2mi_found
                                         ; t2mi_lost
                                         ; t2mi_errors
                                         ; board_errors
                                         ; bitrate
                                         ; structs
                                         ; t2mi_info
                                         ; jitter } in
    let push_events                   = { status       = status_push
                                        ; ts_found     = ts_found_push
                                        ; ts_lost      = ts_lost_push
                                        ; ts_errors    = ts_errors_push
                                        ; t2mi_found   = t2mi_found_push
                                        ; t2mi_lost    = t2mi_lost_push
                                        ; t2mi_errors  = t2mi_errors_push
                                        ; board_errors = board_errors_push
                                        ; bitrate      = bitrate_push
                                        ; structs      = structs_push
                                        ; t2mi_info    = t2mi_info_push
                                        ; jitter       = jitter_push
                                        } in
    let api = { set_mode        = (fun m  -> enqueue_instant imsgs sender (Set_board_mode m))
              ; set_jitter_mode = (fun m  -> enqueue_instant imsgs sender (Set_jitter_mode m))
              ; get_section     = (fun () -> enqueue msgs sender
                                                     (Get_section (get_id (),
                                                                   { stream_id = T2mi_plp 0
                                                                   ; section   = 0
                                                                   ; table     =  PAT { common = { version = 0
                                                                                                 ; id      = 0
                                                                                                 ; pid     = 0
                                                                                                 ; lsn     = 1
                                                                                                 ; section_syntax = false
                                                                                                 ; sections = []}
                                                                                      ; ts_id  = 1 }}))
                                                     (to_period 10 step_duration)
                                                     None)
              ; get_t2mi_seq    = (fun s  -> enqueue msgs sender
                                                     (Get_t2mi_frame_seq { request_id = get_id ()
                                                                         ; seconds    = s })
                                                     (to_period (s + 10) step_duration)
                                                     (Some T2mi_seq_timeout))
              ; reset           = (fun () -> enqueue_instant imsgs sender Reset)
              ; config          = (fun () -> Lwt.return storage#get)
              } in
    events,
    api,
    (step msgs imsgs sender storage step_duration push_state push_events)

end
