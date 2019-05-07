open Board_niitv_tsan_types
open Application_types

type api_msg = (Cstruct.t Lwt_stream.t -> unit Lwt.t)

let ( >>= ) = Lwt.( >>= )

let cooldown_timeout = 10.

let status_timeout = 8.

let log_error (type a) src (req : a Request.t) (error : Request.error) =
  Logs.err ~src (fun m ->
      m "Request \"%s\" failed. Error = %s"
        (Request.to_string req)
        (Request.error_to_string error))

let log_ok (type a) src (req : a Request.t) v =
  Logs.debug ~src (fun m ->
      let base =
        Printf.sprintf "Request \"%s\" succeeded"
          (Request.to_string req) in
      let s = match Request.value_to_string req v with
        | None -> base
        | Some v -> Printf.sprintf "%s. Response = %s" base v in
      m "%s" s)

let sleep timeout =
  Lwt_unix.sleep timeout
  >>= fun () -> Lwt.return_error Request.Timeout

let loop (type a)
    (stream : Request.rsp Lwt_stream.t)
    (req : a Request.t) : (a, Request.error) result Lwt.t =
  let rec aux () =
    Lwt_stream.next stream
    >>= fun x ->
    match Parser.is_response req x with
    | None -> aux ()
    | Some (Ok _ as x) -> Lwt.return x
    | Some (Error _ as e) -> Lwt.return e in
  aux ()

let request (type a)
    (src : Logs.src)
    (stream : Request.rsp_tag Request.msg Lwt_stream.t)
    (sender : Cstruct.t -> unit Lwt.t)
    (req : a Request.t) : (a, Request.error) result Lwt.t =
  sender @@ Serializer.serialize req
  >>= fun () ->
  let wait req = Lwt.pick [loop stream req; sleep (Request.timeout req)] in
  (match req with
   | Get_devinfo -> wait req
   | Get_deverr _ -> wait req
   | Get_mode -> wait req
   | Set_mode _ -> Lwt.return_ok ()
   | Set_jitter_mode _ -> Lwt.return_ok ()
   | Reset -> Lwt.return_ok ()
   | Set_src_id _ -> Lwt.return_ok ()
   | Get_t2mi_seq _ -> wait req
   | Get_section _ -> wait req
   | Get_jitter _ -> wait req
   | Get_bitrate _ -> wait req
   | Get_structure _ -> wait req
   | Get_t2mi_info _ -> wait req)
  >>= function
  | Error e -> log_error src req e; Lwt.return_error e
  | Ok x -> log_ok src req x; Lwt.return_ok x

let start
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (req_queue : api_msg Lwt_stream.t)
    (rsp_queue : Request.rsp Lwt_stream.t)
    (kv : config Kv_v.rw)
    (set_state : Topology.state -> unit)
    (set_devinfo : devinfo -> unit) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let rec first_step () =
    Logs.info (fun m -> m "Start of connection establishment...");
    Lwt_stream.junk_old req_queue
    >>= fun () -> Lwt_stream.junk_old rsp_queue
    >>= fun () -> set_state `No_response; detect_device ()

  and restart () =
    Logs.info (fun m -> m "Restarting...");
    set_state `No_response;
    Lwt_stream.junk_old req_queue
    >>= fun () -> Lwt_stream.junk_old rsp_queue
    >>= fun () -> Lwt_unix.sleep cooldown_timeout
    >>= first_step

  and detect_device () =
    print_endline "detect device";
    let rec loop () =
      Lwt_stream.next rsp_queue
      >>= function
      | `Simple { tag = `Status; _ } -> request src rsp_queue sender Request.Get_devinfo
      | `Simple { tag = `Devinfo; data } ->
        (match Parser.parse_devinfo data with
         | Error _ as e -> Lwt.return e
         | Ok info as x ->
           Logs.debug (fun m -> m "Got device info event: %a" pp_devinfo info);
           Lwt.return x)
      | _ -> loop () in
    loop ()
    >>= function
    | Ok x -> init_device x
    | Error e ->
      Logs.err (fun m ->
          m "Got error during detect step: %s"
          @@ Request.error_to_string e);
      restart ()

  and init_device (info : devinfo) =
    set_state `Init;
    set_devinfo info;
    Logs.info (fun m -> m "Connection established, device initialization started...");
    restart ()
    (* let rec loop () =
     *   Lwt_stream.next rsp_queue
     *   >>= function
     *   | { tag = `Status; _ } -> request src rsp_queue sender Request.Get_devinfo
     *   | { tag = `Devinfo_rsp; data } -> Lwt.return @@ Parser.parse_devinfo data
     *   | _ -> loop () in
     * loop ()
     * >>= function
     * | Ok x -> init_device x
     * | Error e ->
     *   Logs.err (fun m ->
     *       m "Got error during detect step: %s"
     *       @@ Request.error_to_string e);
     *   restart () *)
  in
  first_step ()
