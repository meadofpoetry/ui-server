open Board_dektec_dtm3200_types

let ( >>= ) = Lwt.( >>= )

let reboot_steps = 10

let log_ok (type a) src (req : a Request.t) v =
  Logs.debug ~src (fun m ->
      m "Request \"%s\" succeeded with response = %s"
        (Request.to_string req) (Request.value_to_string req v))

let log_error (type a) src (req : a Request.t) (error : error) =
  Logs.warn ~src (fun m ->
      m "Request \"%s\" failed with error = %s"
        (Request.to_string req) (error_to_string error))

let sleep timeout =
  Lwt_unix.sleep timeout
  >>= fun () -> Lwt.return_error Timeout

let loop (type a) stream (req : a Request.t) : (a, error) result Lwt.t =
  let rec aux () =
    Lwt_stream.next stream
    >>= fun x ->
    match Parser.is_response req x with
    | None -> aux ()
    | Some (Ok x) -> Lwt.return_ok x
    | Some (Error e) -> Lwt.return_error (Fail e) in
  Lwt_stream.junk_old stream >>= aux
