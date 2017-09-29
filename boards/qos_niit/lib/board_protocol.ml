open Board_types
open Lwt.Infix
open Storage.Options
open Api.Handler
open Meta_board

include Board_parser

let io x = Lwt_io.printf "%s\n" x |> ignore

let to_period x step_duration  = x * int_of_float (1. /. step_duration)

module SM = struct

  let request_id = ref (-1)

  exception T2mi_seq_timeout
  exception Streams_timeout

  let get_id ()  = incr request_id; !request_id

  let wakeup_timeout t = t.pred `Timeout |> ignore

  type events = { status       : user_status React.event
                ; ts_sync      : (Common.Stream.t * bool) React.event
                ; ts_errors    : ts_errors React.event
                ; t2mi_sync    : (int * bool) React.event
                ; t2mi_errors  : t2mi_errors React.event
                ; board_errors : board_errors React.event
                ; bitrate      : bitrate list React.event
                ; structs      : ts_struct list React.event
                ; t2mi_info    : t2mi_info list React.event
                ; jitter       : jitter React.event
                }

  type push_events = { status       : user_status              -> unit
                     ; ts_sync      : (Common.Stream.t * bool) -> unit
                     ; ts_errors    : ts_errors                -> unit
                     ; t2mi_sync    : int * bool               -> unit
                     ; t2mi_errors  : t2mi_errors              -> unit
                     ; board_errors : board_errors             -> unit
                     ; bitrate      : bitrate list             -> unit
                     ; structs      : ts_struct list           -> unit
                     ; t2mi_info    : t2mi_info list           -> unit
                     ; jitter       : jitter                   -> unit
                     }

  module Msg_group : sig
    type t
    type msgs = [ `Status of Board_types.status
                | `T2mi_errors of Cbuffer.t
                | `Ts_errors of Cbuffer.t ]
    val extract       : msgs list -> t option * msgs list
    val extract_all   : msgs list -> t list * msgs list
    val get_req_stack : t -> t option -> event event_request list
    val push          : t -> t option -> event list -> push_events -> t
  end = struct

    type t =
      { status      : status
      ; streams     : Common.Stream.t list
      ; ts_errors   : ts_errors list
      ; t2mi_errors : t2mi_errors list
      ; jitter_ptr  : int32
      }
    type msgs = [ `Status of Board_types.status
                | `T2mi_errors of Cbuffer.t
                | `Ts_errors of Cbuffer.t ]

    let find_status       = CCList.find_idx (function | `Status _ -> true | _ -> false)

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

    let extract events =
      match find_status events with
      | Some (id,_) -> let _,events' = CCList.take_drop id events in
                       (match events' with
                        | (`Status x) :: tl -> (match find_status tl with
                                                | Some (id,_) ->
                                                   let gp,rest = CCList.take_drop id tl in
                                                   (Some { status      = x
                                                         ; ts_errors   = group_ts_errs gp |> sort_ts_errs
                                                         ; t2mi_errors = group_t2mi_errs gp
                                                         ; streams     = []
                                                         ; jitter_ptr  = -1l }, rest)
                                                | None -> None,events)
                        | _ -> None,events)
      | None -> None,events

    let extract_all events =
      let rec f = fun acc x -> match extract x with
                               | Some gp,rest -> f (gp :: acc) rest
                               | None,rest    -> acc,rest in
      let gps,rest = f [] events in
      List.rev gps,rest

    let get_req_stack gp old_gp =
      let version        = gp.status.streams_ver in
      let bitrate_req    = Get_bitrates { request_id = get_id (); version} in
      let jitter_req     = Get_jitter { request_id = get_id (); pointer = gp.jitter_ptr } in
      let board_errs_req = if gp.status.has_board_errs then [ (Get_board_errors (get_id ())) ] else [] in
      [ bitrate_req; jitter_req ]
      @ board_errs_req
      @ (match old_gp with
         | Some old_gp -> (if old_gp.status.ts_ver_com <> gp.status.ts_ver_com
                           then [ Get_ts_structs { request_id = get_id (); version } ] else [])
         (* @ (List.map (fun (id,ver) -> Get_t2mi_info { request_id     = get_id () *)
         (*                                            ; version        = ver *)
         (*                                            ; t2mi_stream_id = id }) *)
         (*             (CCList.foldi (fun acc i x -> if x = (CCList.nth gp.status.t2mi_ver_lst i) *)
         (*                                           then acc *)
         (*                                           else (i,x) :: acc) *)
         (*                           [] old_gp.status.t2mi_ver_lst)) *)
         | None -> (if CCList.is_empty gp.status.ts_sync_lst then []
                    else [ Get_ts_structs { request_id = get_id (); version } ])
        (* @ (if CCList.is_empty gp.status.t2mi_sync_lst then [] *)
        (*    else (List.map (fun id -> Get_t2mi_info { request_id  = get_id () *)
        (*                                            ; version = CCList.nth gp.status.ts_ver_lst id *)
        (*                                            ; t2mi_stream_id = id }) *)
        (*                   gp.status.t2mi_sync_lst)) *))

    let push gp _ (rsps : event list) (pe : push_events) =
      let streams    = CCOpt.get_exn @@ CCList.find_map (function
                                                         | (Bitrate (x,_) | Struct (x,_)) -> Some x.streams
                                                         | _                              -> None) rsps in
      let jitter_ptr = CCOpt.get_exn @@ CCList.find_map (function
                                                         | Jitter x -> Some x.next_ptr
                                                         | _        -> None ) rsps in
      let new_sync = CCList.map2 CCPair.make streams gp.status.ts_sync_lst in
      let sync_lost,sync_found = CCList.partition_map (fun ((_,sync) as x) ->
                                     if sync then `Right x else `Left x) new_sync in
      pe.status gp.status.user_status;
      List.iter pe.ts_sync sync_found;
      List.iter pe.ts_errors gp.ts_errors;
      List.iter pe.t2mi_errors gp.t2mi_errors;
      List.iter pe.ts_sync sync_lost;
      List.iter (function
                 | Board_errors x -> pe.board_errors x
                 | Bitrate (_,x)  -> pe.bitrate x
                 | Struct  (_,x)  -> pe.structs x
                 | T2mi_info x    -> pe.t2mi_info x
                 | Jitter x       -> pe.jitter x
                 | _              -> ()) rsps;
      { gp with streams = streams; jitter_ptr = jitter_ptr }

  end

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    (match msg with
     | Get_board_info                  -> to_common_header ~msg_code:0x0080 ()
     | Get_board_mode                  -> to_common_header ~msg_code:0x0081 ()
     | Get_t2mi_frame_seq (id,seconds) -> let body = Cbuffer.create sizeof_req_get_t2mi_frame_seq in
                                          let ()   = set_req_get_t2mi_frame_seq_time body seconds in
                                          to_complex_req ~request_id:id ~msg_code:0x0306 ~body ())
    |> sender

  let send_event (type a) sender (msg : a event_request) : unit Lwt.t =
    (match msg with
     | Get_board_errors id -> to_complex_req ~request_id:id
                                             ~msg_code:0x0110
                                             ~body:(Cbuffer.create 0) ()
     | Get_jitter req      -> let body = Cbuffer.create sizeof_req_get_jitter in
                              let ()   = set_req_get_jitter_ptr body req.pointer in
                              to_complex_req ~request_id:req.request_id
                                             ~msg_code:0x0307
                                             ~body ()
     | Get_ts_structs req  -> let stream = (Common.Stream.of_int32 0xFFFFFFFFl) in
                              let body = Cbuffer.create sizeof_req_get_ts_struct in
                              let ()   = set_req_get_ts_struct_stream_id body @@ Common.Stream.to_int32 stream in
                              to_complex_req ~request_id:req.request_id
                                             ~msg_code:0x0309
                                             ~body ()
     | Get_bitrates req    -> to_complex_req ~request_id:req.request_id ~msg_code:0x030A ~body:(Cbuffer.create 0) ()
     | Get_t2mi_info req   -> let body = Cbuffer.create sizeof_req_get_t2mi_info in
                              let ()   = set_req_get_t2mi_info_stream_id body req.t2mi_stream_id in
                              to_complex_req ~request_id:req.request_id
                                             ~msg_code:0x030B
                                             ~body ()
     | Get_streams req     -> to_complex_req ~request_id:req.request_id
                                             ~msg_code:0x030C
                                             ~body:(Cbuffer.create 0) ())
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
    msgs := Msg_queue.append !msgs { send; pred; timeout; exn };
    t

  let enqueue_instant (type a) msgs sender (msg : a instant_request) : unit Lwt.t =
    let t,w = Lwt.wait () in
    let send = fun () -> (send_instant sender msg) >>= (fun x -> Lwt.return @@ Lwt.wakeup w x) in
    let pred = fun _  -> None in
    msgs := Msg_queue.append !msgs { send; pred; timeout = 0; exn = None };
    t

  let step msgs imsgs sender (storage : config storage) step_duration push_state push_events =
    let period         = to_period 5 step_duration in
    (* let section_period = to_period 120 step_duration in *)

    let rec first_step () =
      Msg_queue.iter !msgs wakeup_timeout;
      msgs := Msg_queue.create [];
      imsgs := Msg_queue.create [];
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
            send_instant sender Reset |> ignore;
            step_normal_begin ())
      else (if p < 0 then first_step ()
            else `Continue (step_detect (pred p) acc))

    and step_normal_begin () =
      push_state `No_response;
      `Continue (step_normal_idle period None [] [] [] None)

    and step_normal_idle p prev_gp prev_events prev_rsps parts acc recvd =
      let events,_,rsps,parts,acc = deserialize parts (Meta_board.concat_acc acc recvd) in
      if CCOpt.is_none @@ CCList.find_map (is_response Get_board_info) rsps
      then
        let events = prev_events @ events in
        (* let rsps   = prev_rsps   @ rsps   in *)
        (match Msg_group.extract events with
         | None,rest_events -> if p < 0
                               then step_normal_begin ()
                               else `Continue (step_normal_idle p prev_gp rest_events [] parts acc)
         | Some gp,rest_events ->
            let pool = Msg_pool.create @@ List.map (fun req -> { send    = (fun () -> send_event sender req)
                                                               ; pred    = (is_event req)
                                                               ; timeout = period
                                                               ; exn     = None
                                                               })
                                                   (Msg_group.get_req_stack gp prev_gp) in
            step_normal_probes_send pool [] prev_gp gp rest_events [] parts acc)
      else first_step ()

    and step_normal_probes_send pool pool_rsps prev_gp gp prev_events prev_rsps parts acc =
      if Msg_pool.empty pool
      then step_normal_probes_finalize pool_rsps prev_gp gp prev_events [] parts acc
      else (Msg_pool.send pool () |> ignore;
            `Continue (step_normal_probes_wait pool pool_rsps period prev_gp gp prev_events prev_rsps parts acc))

    and step_normal_probes_wait pool pool_rsps p prev_gp gp prev_events prev_rsps parts acc recvd =
      let events,ev_rsps,rsps,parts,acc = deserialize parts (Meta_board.concat_acc acc recvd) in
      let events = prev_events @ events in
      let rsps   = prev_rsps   @ rsps   in
      try
        (match Msg_pool.responsed pool ev_rsps with
         | None   -> let pool = Msg_pool.step pool in
                     `Continue (step_normal_probes_wait pool pool_rsps (pred p) prev_gp gp events rsps parts acc)
         | Some x -> let new_pool = Msg_pool.next pool in
                     let pool_rsps = x :: pool_rsps in
                     if Msg_pool.last pool
                     then step_normal_probes_finalize pool_rsps prev_gp gp events rsps parts acc
                     else step_normal_probes_send new_pool pool_rsps prev_gp gp events rsps parts acc)
      with
      | Timeout -> io "exit by timeout"; step_normal_begin ()

    and step_normal_probes_finalize pool_rsps prev_gp gp prev_events prev_rsps parts acc =
      push_state `Fine;
      let gp = Msg_group.push gp prev_gp pool_rsps push_events in
      `Continue (step_normal_idle period (Some gp) prev_events prev_rsps parts acc)

    in first_step ()

  let create sender (storage : config storage) push_state step_duration =
    let period = to_period 5 step_duration in
    let msgs   = ref (Msg_queue.create []) in
    let imsgs  = ref (Msg_queue.create []) in
    let status,status_push             = React.E.create () in
    let ts_sync,ts_sync_push           = React.E.create () in
    let ts_errors,ts_errors_push       = React.E.create () in
    let t2mi_sync,t2mi_sync_push       = React.E.create () in
    let t2mi_errors,t2mi_errors_push   = React.E.create () in
    let board_errors,board_errors_push = React.E.create () in
    let bitrate,bitrate_push           = React.E.create () in
    let structs,structs_push           = React.E.create () in
    let t2mi_info,t2mi_info_push       = React.E.create () in
    let jitter,jitter_push             = React.E.create () in
    let (events : events)              = { status
                                         ; ts_sync
                                         ; ts_errors
                                         ; t2mi_sync
                                         ; t2mi_errors
                                         ; board_errors
                                         ; bitrate
                                         ; structs
                                         ; t2mi_info
                                         ; jitter } in
    let push_events                   = { status       = status_push
                                        ; ts_sync      = ts_sync_push
                                        ; ts_errors    = ts_errors_push
                                        ; t2mi_sync    = t2mi_sync_push
                                        ; t2mi_errors  = t2mi_errors_push
                                        ; board_errors = board_errors_push
                                        ; bitrate      = bitrate_push
                                        ; structs      = structs_push
                                        ; t2mi_info    = t2mi_info_push
                                        ; jitter       = jitter_push
                                        } in
    let api = { devinfo         = (fun () -> enqueue msgs sender Get_board_info period None)
              ; set_mode        = (fun m  -> enqueue_instant imsgs sender (Set_board_mode m))
              ; set_jitter_mode = (fun m  -> enqueue_instant imsgs sender (Set_jitter_mode m))
              ; get_t2mi_seq    = (fun s  -> enqueue msgs sender
                                                     (Get_t2mi_frame_seq (get_id (),s))
                                                     (s + 10)
                                                     (Some T2mi_seq_timeout))
              ; reset           = (fun () -> enqueue_instant imsgs sender Reset)
              ; config          = (fun () -> Lwt.return storage#get)
              } in
    events,
    api,
    (step msgs imsgs sender storage step_duration push_state push_events)

end
