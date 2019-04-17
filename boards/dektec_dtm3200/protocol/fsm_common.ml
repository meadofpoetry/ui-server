let ( >>= ) = Lwt.( >>= )

let reboot_steps = 10

let log_ok (type a) src (req : a Request.t) v =
  Logs.debug ~src (fun m ->
      m "Request \"%s\" succeeded. Response = %s"
        (Request.to_string req) (Request.value_to_string req v))

let log_error (type a) src (req : a Request.t) (error : Request.error) =
  Logs.err ~src (fun m ->
      m "Request \"%s\" failed. Error = %s"
        (Request.to_string req) (Request.error_to_string error))

let sleep timeout =
  Lwt_unix.sleep timeout
  >>= fun () -> Lwt.return_error Request.Timeout

let loop (type a) stream (req : a Request.t) : (a, Request.error) result Lwt.t =
  let rec aux () =
    Lwt_stream.next stream
    >>= fun x ->
    match Parser.is_response req x with
    | None -> aux ()
    | Some (Ok x) -> Lwt.return_ok x
    | Some (Error e) -> Lwt.return_error e in
  Lwt_stream.junk_old stream >>= aux

let request (type a) ~address src sender stream (req : a Request.t) =
  sender @@ Serializer.make_req ~address req
  >>= fun () ->
  Lwt.pick [loop stream req; sleep (Request.timeout req)]
  >>= function
  | Error e -> log_error src req e; Lwt.return_error e
  | Ok x -> log_ok src req x; Lwt.return_ok x
