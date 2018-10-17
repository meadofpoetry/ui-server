open Components
open Lwt_result.Infix

let get_board_state control =
  Requests.Device.HTTP.get_state control
  >|= (fun state ->
    let e, sock = Requests.Device.WS.get_state control in
    React.S.hold state e, sock)
  |> Lwt_result.map_err Api_js.Requests.err_to_string

let get_stream id control =
  Requests.Streams.HTTP.get_streams ~ids:[id] control
  >|= (fun streams ->
    let e, sock = Requests.Streams.WS.get_streams ~ids:[id] control in
    React.S.hold streams e, sock)
  |> Lwt_result.map_err Api_js.Requests.err_to_string

let get_state id control =
  let open Widget_common in
  get_board_state control
  >>= fun (state, state_sock) -> get_stream id control
  >>= (fun (stream, stream_sock) ->
    React.S.l2 (fun state stream ->
        match state, stream with
        | `No_response, _ -> No_response
        | `Init, _ -> No_response
        | _, [] -> No_sync
        | _ -> Fine) state stream
    |> fun s -> Lwt_result.return (s, (fun () ->
                      React.S.stop ~strong:true s;
                      state_sock##close;
                      stream_sock##close)))
