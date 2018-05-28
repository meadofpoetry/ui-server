open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect

(**
 ** API
 **
 ** POST /device/mode/[t2mi|jitter]
 ** POST /device/port/{port}/{boolean}
 ** POST /device/reset
 ** GET  /device/info
 ** GET  /device/mode/[t2mi|jitter]
 ** GET  /device/state
 ** GET  /device/status
 ** GET  /device/errors
 **
 ** QUERY PARAMETERS (for 'errors','status','state' requests)
 **
 ** [from]      - timestamp (can be 'now', 'now' - timespan)
 ** [to]        - timestamp (can be 'now')
 ** [f[errors]] - list of error codes to be filtered (for 'errors' request only)
 ** [f[fine,init,no-response]] - list of states to be filtered (for 'state' request only)
 ** [limit]     - maximum number of items in a response (default FIXME)
 ** [total]     - include [total] value into response to know how many collection items are available
 ** [decimate]  - if true, decimate the number of items in a collection  (e.g. for charts).
 **               Possible values: 'off',FIXME mention available decimation algorithms here
 **
 **)

include Api_utils.Device

let bad_request     = respond_error ~status:`Bad_request
let not_implemented = respond_error ~status:`Not_implemented

module WS = struct

  let state sock_data (events:events) body () =
    sock_handler sock_data (React.S.changes events.state) Common.Topology.state_to_yojson body

  let status sock_data (events:events) body () =
    sock_handler sock_data events.status status_to_yojson body

  let errors sock_data (events:events) body () =
    sock_handler sock_data events.board_errors board_errors_to_yojson body

  let mode mode sock_data (events:events) body () =
    let f = fun e conv -> sock_handler sock_data e conv body in
    match mode with
    | `T2MI   -> let e = React.E.map (fun x -> x.t2mi_mode) events.config
                         |> React.E.changes ~eq:(Equal.option equal_t2mi_mode)
                 in f e t2mi_mode_request_to_yojson
    | `JITTER -> let e = React.E.map (fun x -> x.jitter_mode) events.config
                         |> React.E.changes ~eq:(Equal.option equal_jitter_mode)
                 in f e jitter_mode_request_to_yojson

end

module REST = struct

  let post_reset (api:api) () =
    api.reset () >|= Result.return
    >>= Json.respond_result_unit

  let post_t2mi_mode (api:api) body () =
    Json.of_body body >>= fun mode ->
    (match t2mi_mode_request_of_yojson mode with
     | Error e -> Lwt_result.fail @@ Json.of_error_string e
     | Ok mode -> api.set_t2mi_mode mode >|= Result.return)
    >>= Json.respond_result_unit

  let post_jitter_mode (api:api) body () =
    Json.of_body body >>= fun mode ->
    (match jitter_mode_request_of_yojson mode with
     | Error e -> Lwt_result.fail @@ Json.of_error_string e
     | Ok mode -> api.set_jitter_mode mode >|= Result.return)
    >>= Json.respond_result_unit

  let post_mode mode (api:api) body () =
    match mode with
    | `T2MI   -> post_t2mi_mode api body ()
    | `JITTER -> post_jitter_mode api body ()

  let post_port port en (api:api) () = match Board_parser.input_of_int port, en with
    | Some i,   true  -> api.set_input i   >|= Result.return >>= Json.respond_result_unit
    | Some ASI, false -> api.set_input SPI >|= Result.return >>= Json.respond_result_unit
    | Some SPI, false -> api.set_input ASI >|= Result.return >>= Json.respond_result_unit
    | _               -> not_found ()

  (** Real-time GET requests **)
  module RT = struct

    let state (events:events) () =
      React.S.value events.state
      |> Common.Topology.state_to_yojson
      |> Result.return
      |> Json.respond_result

    let devinfo api () =
      api.get_devinfo () >|= (devinfo_response_to_yojson %> Result.return)
      >>= Json.respond_result

    let mode mode (api:api) () =
      (match mode with
       | `T2MI   -> api.config () >|= (fun x -> Ok (t2mi_mode_request_to_yojson x.t2mi_mode))
       | `JITTER -> api.config () >|= (fun x -> Ok (jitter_mode_request_to_yojson x.jitter_mode)))
      >>= Json.respond_result

  end

  (** Archive GET requests **)
  module AR = struct

    (* FIXME implement *)
    let state () =
      not_implemented "state archive" ()

    (* FIXME implement *)
    let status () =
      not_implemented "status archive" ()

    (* FIXME implement *)
    let errors () =
      not_implemented "status archive" ()

  end

end

let handle api events scheme meth req uri sock_data body () =
  let open Result.Infix in
  match scheme,meth,req,`Now with
  (* POST *)
  | _,`POST,`Mode m,_     -> REST.post_mode m api body ()
  | _,`POST,`Port (p,b),_ -> REST.post_port p b api ()
  | _,`POST,`Reset,_      -> REST.post_reset api ()
  (* Websockets real-time *)
  | `WS,`GET,`Errors,`Now -> WS.errors sock_data events body ()
  | `WS,`GET,`Mode m,`Now -> WS.mode m sock_data events body ()
  | `WS,`GET,`State, `Now -> WS.state  sock_data events body ()
  | `WS,`GET,`Status,`Now -> WS.status sock_data events body ()
  (* Websosckets archive *)
  | `WS,`GET,_,`Past _    -> not_implemented "WS archive REQ is not implemented" ()
  (* RESTful GET real-time *)
  | `REST,`GET,`Mode m,`Now -> REST.RT.mode m api ()
  | `REST,`GET,`Info,  `Now -> REST.RT.devinfo api ()
  | `REST,`GET,`State, `Now -> REST.RT.state events ()
  | `REST,`GET,`Errors,`Now -> not_implemented "REST real-time REQ is not implemented" ()
  | `REST,`GET,`Status,`Now -> not_implemented "REST real-time REQ is not implemented" ()
  (* RESTful GET archive *)
  | `REST,`GET,`Errors,`Past t -> REST.AR.errors ()
  | `REST,`GET,`Status,`Past t -> REST.AR.status ()
  | `REST,`GET,`State, `Past t -> REST.AR.state ()
  | _ -> not_found ()
