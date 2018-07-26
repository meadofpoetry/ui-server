open Containers
open Board_types
open Types
open Storage.Options
open Boards.Board
open Boards.Pools
open Common
open Board_parser

let status_timeout = 8 (* seconds *)
let detect_timeout = 3 (* seconds *)
let probes_timeout = 5 (* seconds *)

module Timer = Boards.Timer

module Acc = struct
  type t =
    { group  : Types.group option
    ; events : Types.event list
    ; probes : probe_response list
    ; parts  : (int * int, part list) List.Assoc.t
    ; bytes  : Cstruct.t option
    }
  let empty = { group = None; events = []; probes = []; parts = []; bytes = None }
end

module SM = struct

  let request_id = ref (-1)
  let jitter_ptr = ref (-1l)

  exception Status_timeout
  exception Unexpected_init

  let get_id () = incr request_id; !request_id

  let wakeup_timeout (_,t) = t.pred `Timeout |> ignore

  let has_board_info rsps = Fun.(Option.is_some % List.find_map (is_response Get_board_info)) rsps

  module Events : sig
    val event_to_string : event -> string
    val group_to_string : group -> string
    val handle          : event list -> Acc.t -> group list * Acc.t
    val update_versions : Acc.t -> group -> Acc.t
    val get_req_stack   : group -> group option -> probe_response probe_request list
    val to_ts_errors    : group -> (Stream.id * Errors.t list) list
    val to_t2mi_errors  : group -> (Stream.id * Errors.t list) list
  end = struct

    let event_to_string : event -> string = function
      | `Status x ->
         Printf.sprintf "status: %s" (show_status_raw x)
      | `Streams_event x ->
         Printf.sprintf "streams: %s" (show_streams x)
      | `T2mi_errors (id,e) ->
         let id = Stream.id_to_int32 id in
         Printf.sprintf "T2-MI errors(stream=%ld,len=%d)" id (List.length e)
      | `Ts_errors (id,e) ->
         let id = Stream.id_to_int32 id in
         Printf.sprintf "TS errors(stream=%ld,len=%d)" id (List.length e)
      | `End_of_errors ->
         "end of errors"

    let group_to_string (g:group) : string =
      let e = String.concat "; " (List.map event_to_string g.events) in
      Printf.sprintf "status(%d), events: [%s]" g.status.version e

    let update_versions (acc:Acc.t) (t:group) : Acc.t = match acc.group with
      | None      -> acc
      | Some from ->
         let group = { t with status = { t.status with versions = from.status.versions }} in
         { acc with group = Some group }

    let split_by l sep =
      let res,acc =
        List.fold_left (fun (res, acc) x ->
            if equal_event x sep
            then ((List.rev acc) :: res), [ ]
            else res, (x :: acc))
          ([ ], [ ]) l
      in (List.rev res), (List.rev acc)

    (** Returns merged groups and accumulator. Last group received is head of list *)
    let handle (events:event list) (acc:Acc.t) =
      let groups,rest = split_by (acc.events @ events) `End_of_errors in
      let groups =
        List.filter (function
            | `Status _ :: `Streams_event _ :: _ -> true
            | _ -> false) groups
        |> List.fold_left (fun (gps:group list) (x:event list) ->
               let prev_status = match gps with
                 | [ ]  -> Option.(acc.group >|= (fun x -> x.status))
                 | x::_ -> Some x.status
               in
               match x with
               | `Status status :: `Streams_event streams :: events ->
                  ({ status = { status with streams }; prev_status; events }:group) :: gps
               | _ -> assert false) []
      in groups,{ acc with events = rest}

    let get_req_stack ({ status; _ }:group) (prev_t:group option) :
          probe_response probe_request list =
      let bitrate = if status.status.has_sync then Some (Get_bitrates (get_id ())) else None in
      let jitter  = match status.jitter_mode with
        | None   -> None
        | Some m ->
           (* request for jitter only if required stream is present *)
           if List.mem ~eq:Common.Stream.equal_id m.stream status.streams
           then Some (Get_jitter { request_id = get_id (); pointer = !jitter_ptr })
           else None in
      (* FIXME commented because board not responding for this request *)
      (* let errors  = if status.errors then Some (Get_board_errors (get_id ())) else None in *)
      let errors = None in
      let ts_structs = match prev_t with
        | Some (old:group) ->
           if old.status.versions.ts_ver_com <> status.versions.ts_ver_com
           then Some (Get_ts_struct { stream = `All; request_id = get_id () })
           else None
        | None ->
           Some (Get_ts_struct { stream = `All; request_id = get_id () }) in
      (* let ts_structs = match prev_t with
       *   | Some old ->
       *      let old_version = old.status.versions.ts_ver_lst in
       *      let old_streams = old.status.streams in
       *      let new_version = status.versions.ts_ver_lst in
       *      let new_streams = status.streams in
       *      let streams =
       *        List.foldi (fun acc i id ->
       *            match List.find_idx (fun x -> Stream.equal_id id x)
       *                    old_streams with
       *            | Some (idx, o) ->
       *               (try
       *                  let ov = List.get_at_idx_exn idx old_version in
       *                  let nv = List.get_at_idx_exn i new_version in
       *                  if ov <> nv then id :: acc else acc
       *                with _ -> acc)
       *            | None -> id :: acc) [] new_streams in
       *      List.map (fun x ->
       *          Get_ts_struct { request_id = get_id ()
       *                        ; stream     = `Single x }) streams
       *   | None -> [ Get_ts_struct { request_id = get_id ()
       *                             ; stream     = `All } ] in *)
      let t2mi_structs = match status.t2mi_mode, prev_t with
        | Some m, Some old ->
           (List.map (fun id -> Get_t2mi_info { request_id = get_id ()
                                              ; stream     = m.stream
                                              ; stream_id  = id })
              (List.foldi (fun acc i x ->
                   if (x <> (List.nth old.status.versions.t2mi_ver_lst i))
                      && List.mem ~eq:(=) i status.t2mi_sync
                   then i :: acc
                   else acc) [] status.versions.t2mi_ver_lst))
        | Some m, None ->
           (if List.is_empty status.t2mi_sync then []
            else (List.map (fun id -> Get_t2mi_info { request_id = get_id ()
                                                    ; stream     = m.stream
                                                    ; stream_id  = id })
                    status.t2mi_sync))
      in
      t2mi_structs
      |> List.cons_maybe ts_structs
      |> List.cons_maybe bitrate
      |> List.cons_maybe jitter
      |> List.cons_maybe errors

    let to_ts_errors (g:group) =
      List.filter_map (function `Ts_errors x -> Some x | _ -> None) g.events

    let to_t2mi_errors (g:group) =
      List.filter_map (function `T2mi_errors x -> Some x | _ -> None) g.events

  end

  let probe_req_to_string = function
    | Get_board_errors _ -> "Board errors" | Get_jitter _   -> "Jitter"
    | Get_ts_struct _    -> "TS structure" | Get_bitrates _ -> "TS bitrates"
    | Get_t2mi_info _    -> "T2-MI info"

  let probe_rsp_to_string = function
    | Board_errors _ -> "Board errors"  | Bitrate _   -> "TS bitrates"
    | Struct _       -> "TS structures" | T2mi_info _ -> "T2-MI info"
    | Jitter _       -> "Jitter"

  let send_msg (type a) sender (msg:a request) : unit Lwt.t =
    (match msg with
     | Get_board_info       -> Get_board_info.serialize ()
     | Get_board_mode       -> Get_board_mode.serialize ()
     | Get_t2mi_frame_seq x -> Get_t2mi_frame_seq.serialize x
     | Get_section x        -> Get_section.serialize x)
    |> sender

  let send_event (type a) sender (msg:a probe_request) : unit Lwt.t =
    (match msg with
     | Get_board_errors id -> Get_board_errors.serialize id
     | Get_jitter req      -> Get_jitter.serialize req
     | Get_ts_struct req   -> Get_ts_structs.serialize req
     | Get_bitrates req    -> Get_bitrates.serialize req
     | Get_t2mi_info req   -> Get_t2mi_info.serialize req)
    |> sender

  let send_instant (type a) sender (msg : a instant_request) : unit Lwt.t =
    (match msg with
     | Reset             -> to_complex_req ~msg_code:0x0111 ~body:(Cstruct.create 0) ()
     | Set_board_mode x  -> to_set_board_mode_req x
     | Set_jitter_mode x -> to_set_jitter_mode_req x)
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

  let enqueue_instant (type a) state msgs sender (msg : a instant_request) : unit Lwt.t =
    let open Lwt.Infix in
    match React.S.value state with
    | `Fine ->
       let t,w = Lwt.wait () in
       let send = fun () -> (send_instant sender msg)
                            >>= (fun x -> Lwt.return @@ Lwt.wakeup w x) in
       let pred = fun _  -> None in
       msgs := Queue.append !msgs { send; pred; timeout = 0; exn = None };
       t
    | _ -> Lwt.fail (Failure "board is not responding")

  let handle_probes (pe:push_events) (acc:Acc.t) =
    let open Streams.T2MI in
    let eq = Stream.equal_id in
    let f (x:structure) : structure option -> structure option = function
      | None   -> Some x
      | Some o -> Some { o with streams = o.streams @ x.streams }  in
    let t2mi_info =
      List.filter_map (function
          | T2mi_info x -> Some x
          | _           -> None) acc.probes
      |> List.fold_left (fun acc (id, s) ->
             List.Assoc.update ~eq ~f:(f s) id []) [] in
    List.iter (function
        | Board_errors x -> pe.board_errors x
        | Struct x       ->
           let open Streams.TS in
           pe.info     @@ List.map (fun (s, x) -> s, x.info) x;
           pe.services @@ List.map (fun (s, x) -> s, x.services) x;
           pe.tables   @@ List.map (fun (s, x) -> s, x.tables) x;
           pe.pids     @@ List.map (fun (s, x) -> s, x.pids) x;
        | Jitter x       -> jitter_ptr := x.next_ptr;
                            pe.jitter x.measures
        | Bitrate x      -> pe.bitrates x
        | T2mi_info _    -> ()) acc.probes;
    pe.t2mi_info t2mi_info;
    { acc with probes = [] }

  let push_state (pe:push_events) state =
    pe.bitrates [];
    pe.state state

  let step msgs imsgs sender (storage:config storage) step_duration
        (pe:push_events) (log_prefix:string) =

    let board_info_err_msg =
      "board info was received during normal \
       operation, restarting..." in
    let no_status_msg t =
      Printf.sprintf "no status received for %d seconds, restarting..."
        (Timer.period t) in

    let module Parser = Board_parser.Make(struct
                            let log_prefix = log_prefix
                          end) in

    let fmt fmt = let fs = "%s" ^^ fmt in Printf.sprintf fs log_prefix in

    let deserialize (acc:Acc.t) recvd =
      let events,probes,rsps,parts,bytes =
        Parser.deserialize acc.parts (concat_acc acc.bytes recvd) in
      events,probes,rsps,{ acc with parts; bytes } in

    let log_groups groups =
      Logs.debug (fun m ->
          let pre = "received group(s)" in
          let gps =
            List.map (fun x -> "[" ^ Events.group_to_string x ^ "]") groups
            |> String.concat "; "
          in m "%s" @@ fmt "%s: %s" pre gps) in

    let log_events (events:event list) =
      Logs.debug (fun m ->
          match events with
          | [] -> ()
          | ev ->
             let pre = "received events" in
             let evs = List.map Events.event_to_string ev
                       |> String.concat "; " in
             m "%s" @@ fmt "%s: [%s]" pre evs) in

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
      Logs.info (fun m -> m "%s" @@ fmt "start of connection establishment...");
      Await_queue.iter !msgs wakeup_timeout;
      msgs  := Await_queue.create [];
      imsgs := Queue.create [];
      push_state pe `No_response;
      send_msg sender Get_board_info |> Lwt.ignore_result;
      `Continue (step_detect (Timer.create ~step_duration detect_timeout) Acc.empty)

    and step_detect (timer:Timer.t) (acc:Acc.t) recvd =
      try
        let _,_,rsps,acc = deserialize acc recvd in
        match List.find_map (is_response Get_board_info) rsps with
        | None      -> `Continue (step_detect (Timer.step timer) acc)
        | Some info ->
           push_state pe `Init;
           pe.devinfo (Some info);
           let ({t2mi_mode;input;jitter_mode}:config) = storage#get in
           send_instant sender (Set_board_mode { t2mi=t2mi_mode; input })
           |> Lwt.ignore_result;
           send_instant sender (Set_jitter_mode jitter_mode)
           |> Lwt.ignore_result;
           Logs.info (fun m -> m "%s" @@ fmt "connection established, waiting \
                                              for 'status' message");
           `Continue (step_ok_idle (Timer.create ~step_duration status_timeout) acc)
      with Timer.Timeout t ->
        (Logs.warn (fun m ->
             let s = fmt "connection is not established after \
                          %d seconds, restarting..." (Timer.period t) in
             m "%s" s);
         first_step ())

    and step_ok_idle (timer:Timer.t) (acc:Acc.t) recvd =
      try
        let events,_,rsps,acc = deserialize acc recvd in
        log_events events;
        if has_board_info rsps then raise_notrace Unexpected_init;
        let () = handle_msgs rsps in
        let () = if not (Queue.empty !imsgs)
                 then Queue.send !imsgs () |> Lwt.ignore_result in
        let () = imsgs := Queue.next !imsgs in
        (match Events.handle events acc with
         | [],acc -> `Continue (step_ok_idle (Timer.step timer) acc)
         | (group::_) as groups,acc ->
            if Option.is_none acc.group
            then (Logs.info (fun m -> m "%s" @@ fmt "initialization done!");
                  push_state pe `Fine);
            log_groups groups;
            List.iter (fun x -> pe.group x) @@ List.rev groups;
            let stack = Events.get_req_stack group acc.group in
            let pool  =
              List.map (fun req ->
                  { send    = (fun () -> send_event sender req)
                  ; pred    = (is_probe_response req)
                  ; timeout = Timer.steps ~step_duration probes_timeout
                  ; exn     = None }) stack
              |> Pool.create in
            Logs.debug (fun m ->
                let pre = "prepared stack of following probe requests" in
                let stk = String.concat "; "
                          @@ List.map probe_req_to_string stack in
                m "%s" @@ fmt "%s: [%s]" pre stk);
            step_ok_probes_send pool (Timer.reset timer)
              { acc with group = Some group })
      with Unexpected_init ->
            Logs.warn (fun m -> m "%s" @@ fmt "%s" board_info_err_msg);
            first_step ()
         | Timer.Timeout t ->
            Logs.warn (fun m -> m "%s" @@ fmt "%s" (no_status_msg t));
            first_step ()

    and step_ok_probes_send pool (timer:Timer.t) (acc:Acc.t) =
      if Pool.empty pool
      then `Continue (step_ok_idle (Timer.reset timer) acc)
      else (Pool.send pool () |> Lwt.ignore_result;
            `Continue (step_ok_probes_wait pool timer acc))

    and step_ok_probes_wait pool (timer:Timer.t) (acc:Acc.t) recvd =
      try
        let events,probes,rsps,acc = deserialize acc recvd in
        log_events events;
        if has_board_info rsps then raise_notrace Unexpected_init;
        let () = handle_msgs rsps in
        let () = if not (Queue.empty !imsgs)
                 then Queue.send !imsgs () |> Lwt.ignore_result in
        let () = imsgs := Queue.next !imsgs in
        let timer,acc = match Events.handle events acc with
          | [],acc           -> (Timer.step timer),acc
          | (hd::_) as l,acc ->
             log_groups l;
             List.iter (fun x -> pe.group x) @@ List.rev l;
             (Timer.reset timer),(Events.update_versions acc hd)
        in
        (match Pool.responsed pool probes with
         | None   ->
            `Continue (step_ok_probes_wait (Pool.step pool) timer acc)
         | Some x ->
            Logs.debug (fun m ->
                let pre = "received probe response of type" in
                let rsp = probe_rsp_to_string x in
                m "%s" @@ fmt "%s: '%s'" pre rsp);
            let acc = { acc with probes = x :: acc.probes } in
            match Pool.last pool with
            | true  -> `Continue (step_ok_idle timer (handle_probes pe acc))
            | false -> step_ok_probes_send (Pool.next pool) timer acc)
      with Unexpected_init ->
            Logs.warn (fun m -> m "%s" @@ fmt "%s" board_info_err_msg);
            first_step ()
         | Timer.Timeout t ->
            Logs.warn (fun m -> m "%s" @@ fmt "%s" (no_status_msg t));
            first_step ()
         | Timeout ->
            Logs.warn (fun m ->
                let s = fmt "timeout while waiting for probe \
                             response, restarting..." in
                m "%s" s);
            first_step ()

    in first_step ()

  open Lwt_react

  let to_raw_streams_s (group:group event) : Stream.stream list signal =
    let conv : t2mi_mode option -> input -> Stream.id -> Stream.stream =
      fun mode i x ->
      { id = `Ts x
      ; description =
          (match x with
           | T2mi_plp x -> Some (Printf.sprintf "T2-MI PLP %d" x)
           | _          -> Some "Входной поток")
      ; typ =
          (match mode with
           | Some m ->
              if Stream.equal_id x m.stream && m.enabled
              then `T2mi else `Ts
           | _ -> `Ts)
      ; source =
          (match x with (* FIXME *)
           | T2mi_plp _ -> Stream Single
           | _          -> Port (match i with SPI -> 0 | ASI -> 1))
      }
    in
    E.map (fun (g:group) ->
        List.map (fun x -> conv g.status.t2mi_mode g.status.input x)
          g.status.streams) group
    |> S.hold ~eq:(Equal.list Stream.equal_stream) []

  let to_config_e (group:group event) : config event =
    E.changes ~eq:equal_config
    @@ E.map (fun (x:group) ->
           { input       = x.status.input
           ; t2mi_mode   = x.status.t2mi_mode
           ; jitter_mode = x.status.jitter_mode }) group

  let create (log_prefix:string) sender (storage:config storage)
        step_duration streams_conv =
    let state,     set_state     = S.create `No_response in
    let devinfo,   set_devinfo   = S.create None in
    let group,     set_group     = E.create () in
    let hw_errors, set_hw_errors = E.create () in
    let ts_info,   set_ts_info   = S.create [] in
    let services,  set_services  = S.create [] in
    let tables,    set_tables    = S.create [] in
    let pids,      set_pids      = S.create [] in
    let bitrates,  set_bitrates  = E.create () in
    let t2mi_info, set_t2mi_info = E.create () in

    let e_pcr,pcr_push     = E.create () in
    let e_pcr_s,pcr_s_push = E.create () in

    let config = S.hold ~eq:equal_config storage#get (to_config_e group) in
    let open Streams.TS in
    let (events:events) =
      { device  =
          { config = S.changes config
          ; input  = S.map ~eq:equal_input (fun (x:config) -> x.input) config
          ; state
          ; status = E.changes ~eq:equal_status
                     @@ E.map (fun (x:group) -> x.status.status) group
          ; reset  = E.map (fun (x:group) ->
                         { timestamp = x.status.status.timestamp }) group
          ; errors = hw_errors
          }
      ; ts   =
          (let map ~eq s =
             S.diff (fun _new old ->
                 List.filter (fun (id, s) ->
                     match List.Assoc.get ~eq:Stream.equal_id id old with
                     | Some x -> not @@ eq x s
                     | None   -> true) _new) s in
           { info     = map ~eq:equal_info ts_info
           ; services = map ~eq:equal_services services
           ; tables   = map ~eq:equal_tables tables
           ; pids     = map ~eq:equal_pids pids
           ; bitrates = bitrates
           ; errors   = E.map Events.to_ts_errors group
          })
      ; t2mi =
          { structures = t2mi_info
          ; errors     = E.map Events.to_t2mi_errors group
          }
      ; streams = streams_conv (to_raw_streams_s group)
      ; jitter  =
          { session = E.changes ~eq:Jitter.equal_session e_pcr_s
          ; jitter  = e_pcr
          }
      } in
    let push_events =
      { devinfo        = set_devinfo
      ; state          = set_state
      ; group          = set_group
      ; board_errors   = set_hw_errors
      ; info           = set_ts_info
      ; services       = set_services
      ; tables         = set_tables
      ; pids           = set_pids
      ; bitrates       = set_bitrates
      ; t2mi_info      = set_t2mi_info
      ; jitter         = pcr_push
      ; jitter_session = pcr_s_push
      } in
    let ()    = Lwt_react.S.keep
                @@ React.S.map (fun c -> storage#store c) config in
    let msgs  = ref (Await_queue.create []) in
    let imsgs = ref (Queue.create []) in
    let wait  = fun ~eq v getter to_req ->
      let open Lwt.Infix in
      let s = S.map ~eq getter config in
      if eq (S.value s) v then Lwt.return v
      else enqueue_instant state imsgs sender (to_req v)
           >>= (fun () -> if eq (S.value s) v then Lwt.return v
                          else Lwt.pick [ E.next @@ S.changes s
                                        ; Lwt_unix.timeout (float_of_int status_timeout)])
           >>= (fun x  -> React.S.stop s;
                          if eq x v then Lwt.return x
                          else Lwt.fail @@ Failure "got unexpected value") in
    let fmt fmt = let fs = "%s" ^^ fmt in Printf.sprintf fs log_prefix in
    let api =
      { set_input = (fun i ->
          Logs.info (fun m ->
              let s = fmt "input switch request: %s" (input_to_string i) in
              m "%s" s);
          wait ~eq:equal_input i (fun (c:config) -> c.input)
            (fun i -> Set_board_mode { t2mi = storage#get.t2mi_mode; input = i }))

      ; set_t2mi_mode = (fun mode ->
        Logs.info (fun m ->
            let md = Option.map_or ~default:"none" show_t2mi_mode mode in
            m "%s" @@ fmt "T2-MI mode change request: %s" md);
        wait ~eq:(Equal.option equal_t2mi_mode) mode (fun c -> c.t2mi_mode)
          (fun m -> Set_board_mode { input = storage#get.input; t2mi = m }))

      ; set_jitter_mode = (fun mode ->
        Logs.info (fun m ->
            let md = Option.map_or ~default:"none" show_jitter_mode mode in
            m "%s" @@ fmt "Jitter mode change request: %s" md);
        wait ~eq:(Equal.option equal_jitter_mode) mode (fun c -> c.jitter_mode)
          (fun m -> Set_jitter_mode m))

      ; reset = (fun () ->
        Logs.info (fun m -> m "%s" @@ fmt "Got reset request");
        enqueue_instant state imsgs sender Reset)

      ; get_section = (fun r ->
        Logs.debug (fun m ->
            let s = show_section_params r in
            m "%s" @@ fmt "Got SI/PSI section request: %s" s);
        enqueue state msgs sender (Get_section { request_id = get_id ()
                                               ; params = r })
          (Timer.steps ~step_duration 125) None)

      ; get_t2mi_seq = (fun params ->
        Logs.debug (fun m -> let s = string_of_int params.seconds ^ " sec" in
                             m "%s" @@ fmt "Got T2-MI sequence request: %s" s);
        enqueue state msgs sender
          (Get_t2mi_frame_seq { request_id = get_id ()
                              ; params })
          (Timer.steps ~step_duration (params.seconds + 10)) None)

      ; get_devinfo = (fun () -> S.value devinfo)
      ; config      = (fun () -> storage#get)
      }
    in
    events,
    api,
    (step msgs imsgs sender storage step_duration push_events log_prefix)

end
