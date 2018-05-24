open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect

include Api_utils.Device

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

    let get_state (events:events) () =
      React.S.value events.state
      |> Common.Topology.state_to_yojson
      |> Result.return
      |> Json.respond_result

    let get_devinfo api () =
      api.get_devinfo () >|= (devinfo_response_to_yojson %> Result.return)
      >>= Json.respond_result

    let get_mode mode (api:api) () =
      (match mode with
       | `T2MI   -> api.config () >|= (fun x -> Ok (t2mi_mode_request_to_yojson x.t2mi_mode))
       | `JITTER -> api.config () >|= (fun x -> Ok (jitter_mode_request_to_yojson x.jitter_mode)))
      >>= Json.respond_result

  end

  (** Archive GET requests **)
  module AR = struct

  end

end

let handle api events scheme meth req uri sock_data body () = match scheme,meth,req with
  (* Websockets *)
  (* NOTE queries are ignored for websocket requests at the moment *)
  | `WS,`GET, `Errors     -> WS.errors sock_data events body ()
  | `WS,`GET, `Mode m     -> WS.mode m sock_data events body ()
  | `WS,`GET, `State      -> WS.state  sock_data events body ()
  | `WS,`GET, `Status     -> WS.status sock_data events body ()
  (* Restful POST *)
  | _  ,`POST,`Mode m     -> REST.post_mode m api body ()
  | _  ,`POST,`Port (p,b) -> REST.post_port p b api ()
  | _  ,`POST,`Reset      -> REST.post_reset api ()
  (* Restful GET *)
  | _  ,`GET, `Errors     -> not_found ()                   (* FIXME implement *)
  | _  ,`GET, `State      -> not_found ()                   (* FIXME implement *)
  | _  ,`GET, `Status     -> not_found ()                   (* FIXME implement *)
  | _  ,`GET, `Mode m     -> REST.RT.get_mode m api ()
  | _  ,`GET, `Info       -> REST.RT.get_devinfo api ()
  | _                     -> not_found ()
