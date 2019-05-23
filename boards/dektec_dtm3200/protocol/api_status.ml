open Board_dektec_dtm3200_types
open Api_util

module Event = struct
  open Util_react

  let get_status (api : Protocol.api) _user _body _env _state =
    let event = E.map status_to_yojson api.notifs.status in
    Lwt.return (`Ev event)
end

let get_status (api : Protocol.api) timeout _user _body _env _state =
  let timeout = match timeout with
    | None -> 3. *. Fsm.status_interval
    | Some x -> (float_of_int x) /. 1000. in
  Lwt.pick
    [ (Boards.Board.await_no_response api.notifs.state >>= not_responding)
    ; (Util_react.E.next api.notifs.status >>= Lwt.return_ok)
    ; (Lwt_unix.sleep timeout >>= fun () -> Lwt.return_error Request.Timeout) ]
  >>=? return_value % status_to_yojson
