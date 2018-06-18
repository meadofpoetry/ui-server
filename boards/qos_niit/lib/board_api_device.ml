open Containers
open Common
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect

(** API
    POST /device/mode/[t2mi|jitter]
    POST /device/port/{port}/{boolean}
    POST /device/reset
    GET  /device/info
    GET  /device/mode/[t2mi|jitter]
    GET  /device/state
    GET  /device/status
    GET  /device/errors *)

type events = device_events

let bad_request     = respond_error ~status:`Bad_request
let not_implemented = respond_error ~status:`Not_implemented

module WS = struct

  let state sock_data (events:events) body () =
    sock_handler sock_data (React.S.changes events.state) Common.Topology.state_to_yojson body

  let status sock_data (events:events) body () =
    sock_handler sock_data events.status status_to_yojson body

  let errors sock_data (events:events) body () =
    sock_handler sock_data events.errors board_errors_to_yojson body

  let mode mode sock_data (events:events) body () =
    let f = fun e conv -> sock_handler sock_data e conv body in
    (match mode with
     | `T2MI   -> let e = React.E.map (fun x -> x.t2mi_mode) events.config
                          |> React.E.changes ~eq:(Equal.option equal_t2mi_mode)
                  in f e t2mi_mode_opt_to_yojson
     | `JITTER -> let e = React.E.map (fun x -> x.jitter_mode) events.config
                          |> React.E.changes ~eq:(Equal.option equal_jitter_mode)
                  in f e jitter_mode_opt_to_yojson)

end

module REST = struct

  let post_reset (api:api) () =
    api.reset () >|= Result.return
    >>= Json.respond_result_unit

  let post_t2mi_mode (api:api) body () =
    Json.of_body body >>= fun mode ->
    (match t2mi_mode_opt_of_yojson mode with
     | Error e -> Lwt_result.fail @@ Json.of_error_string e
     | Ok mode -> api.set_t2mi_mode mode >|= Result.return)
    >>= Json.respond_result_unit

  let post_jitter_mode (api:api) body () =
    Json.of_body body >>= fun mode ->
    (match jitter_mode_opt_of_yojson mode with
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
      api.get_devinfo () >|= (devinfo_opt_to_yojson %> Result.return)
      >>= Json.respond_result

    let mode mode (api:api) () =
      (match mode with
       | `T2MI   -> api.config () >|= (fun x -> Ok (t2mi_mode_opt_to_yojson x.t2mi_mode))
       | `JITTER -> api.config () >|= (fun x -> Ok (jitter_mode_opt_to_yojson x.jitter_mode)))
      >>= Json.respond_result

  end

  (** Archive GET requests **)
  module AR = struct

    let state time (q:Uri.Query.t) () =
      respond_error ~status:`Not_implemented "not impelemented" ()

    let status time (q:Uri.Query.t) () =
      respond_error ~status:`Not_implemented "not impelemented" ()

    let errors time (q:Uri.Query.t) () =
      respond_error ~status:`Not_implemented "not impelemented" ()

  end

end

let mode_handler api events id meth ({scheme;path;query}:Uri.sep) sock_data _ body =
  let is_guest = Common.User.eq id `Guest in
  let mode = match Uri.Path.to_string path with
    | "t2mi" -> Some `T2MI | "jitter" -> Some `JITTER | _ -> None
  in
  match Uri.Scheme.is_ws scheme,meth,mode with
  | false,`POST,Some mode -> redirect_if is_guest @@ REST.post_mode mode api body
  | true, `GET, Some mode -> WS.mode mode sock_data events body ()
  | false,`GET, Some mode -> REST.RT.mode mode api ()
  | _ -> not_found ()

let port_handler api id meth ({scheme;path;query}:Uri.sep) _ _ body =
  let is_guest   = Common.User.eq id `Guest in
  let port,path  = Pair.map1 (Option.flat_map int_of_string_opt)  @@ Uri.Path.next path in
  let state,path = Pair.map1 (Option.flat_map bool_of_string_opt) @@ Uri.Path.next path in
  match Uri.Scheme.is_ws scheme,meth,path,port,state with
  | false,`POST,[],Some p,Some b ->
     redirect_if is_guest @@ REST.post_port p b api
  | _ -> not_found ()

let reset_handler api id meth ({scheme;path;query}:Uri.sep) _ _ _ =
  let is_guest = Common.User.eq id `Guest in
  match Uri.Scheme.is_ws scheme,meth,Uri.Path.to_string path with
  | false,`POST,"" -> redirect_if is_guest @@ REST.post_reset api
  | _              -> not_found ()

let info_handler api _ meth ({scheme;path;query}:Uri.sep) _ _ _ =
  match Uri.Scheme.is_ws scheme,meth,Uri.Path.to_string path with
  | false,`GET,"" -> REST.RT.devinfo api ()
  | _ -> not_found ()

let state_handler events _ meth ({scheme;path;query}:Uri.sep) sock_data _ body =
  match Uri.Scheme.is_ws scheme,meth,path with
  | true, `GET,[] -> WS.state sock_data events body ()
  | false,`GET,[] ->
     (match Api.Query.Time.get query with
      | Ok (None,query)        -> REST.RT.state events ()
      | Ok ((Some time),query) -> REST.AR.state time query ()
      | Error e                -> respond_error (Uri.Query.err_to_string e) ())
  | _ -> not_found ()

let status_handler events _ meth ({scheme;path;query}:Uri.sep) sock_data _ body =
  match Uri.Scheme.is_ws scheme,meth,Uri.Path.to_string path with
  | true, `GET,"" -> WS.status sock_data events body ()
  | false,`GET,"" ->
     (match Api.Query.Time.get query with
      | Ok (None,_)   -> not_implemented "FIXME" ()
      | Ok (Some t,q) -> REST.AR.status t q ()
      | Error e       -> respond_error (Uri.Query.err_to_string e) ())
  | _ -> not_found ()

let errors_handler events _ meth ({scheme;path;query}:Uri.sep) sock_data _ body =
  match Uri.Scheme.is_ws scheme,meth,Uri.Path.to_string path with
  | true, `GET,"" -> WS.errors sock_data events body ()
  | false,`GET,"" ->
     (match Api.Query.Time.get query with
      | Ok (None,_)   -> not_implemented "FIXME" ()
      | Ok (Some t,q) -> REST.AR.errors t q ()
      | Error e       -> respond_error (Uri.Query.err_to_string e) ())
  | _ -> not_found ()

let handlers api events =
  [ (module struct
       let domain = "mode"
       let handle = mode_handler api events
     end : Api_handler.HANDLER)
  ; (module struct
       let domain = "port"
       let handle = port_handler api
     end : Api_handler.HANDLER)
  ; (module struct
       let domain = "reset"
       let handle = reset_handler api
     end : Api_handler.HANDLER)
  ; (module struct
       let domain = "info"
       let handle = info_handler api
     end : Api_handler.HANDLER)
  ; (module struct
       let domain = "state"
       let handle = state_handler events
     end : Api_handler.HANDLER)
  ; (module struct
       let domain = "status"
       let handle = status_handler events
     end : Api_handler.HANDLER)
  ; (module struct
       let domain = "errors"
       let handle = errors_handler events
     end : Api_handler.HANDLER)
  ]
