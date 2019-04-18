open Board_niitv_ts2ip_types

type api_msg = (Request.msg Lwt_stream.t -> unit Lwt.t) * (Request.error -> unit)

let ( >>= ) = Lwt.( >>= )

let cooldown_timeout = 10.

let take_drop (n : int) (l : 'a list) =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | l when i = 0 -> List.rev acc, l
    | hd :: tl -> aux (pred i) (hd :: acc) tl
  in
  aux n [] l

let log_ok (type a) src (req : a Request.t) v =
  Logs.debug ~src (fun m ->
      let base =
        Printf.sprintf "Request \"%s\" succeeded"
          (Request.to_string req) in
      let s = match Request.value_to_string req v with
        | None -> base
        | Some v -> Printf.sprintf "%s. Response = %s" base v in
      m "%s" s)

let log_error (type a) src (req : a Request.t) (error : Request.error) =
  Logs.err ~src (fun m ->
      m "Request \"%s\" failed. Error = %s"
        (Request.to_string req) (Request.error_to_string error))

let loop (type a)
    (stream : Request.msg Lwt_stream.t)
    (req : a Request.t) : (a, Request.error) result Lwt.t =
  let rec aux () =
    Lwt_stream.next stream
    >>= fun x ->
    match Parser.is_response req x with
    | None -> aux ()
    | Some (Ok x) -> Lwt.return_ok x
    | Some (Error e) -> Lwt.return_error e in
  (* FIXME remove junk old? *)
  Lwt_stream.junk_old stream >>= aux

let sleep timeout =
  Lwt_unix.sleep timeout
  >>= fun () -> Lwt.return_error Request.Timeout

let request (type a)
    (src : Logs.src)
    (stream : Request.msg Lwt_stream.t)
    (sender : Cstruct.t -> unit Lwt.t)
    (req : a Request.t) : (a, Request.error) result Lwt.t =
  sender @@ Serializer.make_req req
  >>= fun () ->
  Lwt.pick [loop stream req; sleep (Request.timeout req)]
  >>= function
  | Error e -> log_error src req e; Lwt.return_error e
  | Ok x -> log_ok src req x; Lwt.return_ok x

let request_instant (type a)
    (sender : Cstruct.t -> unit Lwt.t)
    (req : unit Request.t) : unit Lwt.t =
  sender @@ Serializer.make_req req

let start (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (req_queue : api_msg Lwt_stream.t)
    (rsp_queue : Request.msg Lwt_stream.t)
    (kv : config Kv_v.rw)
    (set_state : Application_types.Topology.state -> unit) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let rec first_step () =
    Logs.info (fun m -> m "Start of connection establishment...");
    let msgs' = Lwt_stream.get_available req_queue in
    List.iter (fun (_, stop) -> stop Request.Not_responding) msgs';
    Lwt_stream.junk_old req_queue
    >>= fun () ->
    set_state `No_response;
    detect_device ()
    >>= init_device

  and restart () =
    Lwt_unix.sleep cooldown_timeout >>= first_step

  and detect_device () =
    Lwt.return ()

  and init_device () =
    kv#get
    >>= fun { mac; mode } ->
    request_instant sender (Set_mac mac)
    >>= fun () ->
    let main_pkrs, rest_pkrs = take_drop Message.n_udp_main mode.udp in
    request_instant sender (Set_mode_main { mode with udp = main_pkrs })
    >>= restart
  in
  first_step
