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

module HTTP = struct

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

  let devinfo api () =
    api.get_devinfo () >|= (devinfo_opt_to_yojson %> Result.return)
    >>= Json.respond_result

  let mode mode (api:api) () =
    (match mode with
     | `T2MI   -> api.config () >|= (fun x -> Ok (t2mi_mode_opt_to_yojson x.t2mi_mode))
     | `JITTER -> api.config () >|= (fun x -> Ok (jitter_mode_opt_to_yojson x.jitter_mode)))
    >>= Json.respond_result

  let state_now (events:events) () =
    React.S.value events.state
    |> Common.Topology.state_to_yojson
    |> Result.return
    |> Json.respond_result

  (** Archive GET requests **)
  module Archive = struct

    let state time (q:Uri.Query.t) () =
      respond_error ~status:`Not_implemented "not impelemented" ()

    let status time (q:Uri.Query.t) () =
      respond_error ~status:`Not_implemented "not impelemented" ()

    let errors time (q:Uri.Query.t) () =
      respond_error ~status:`Not_implemented "not impelemented" ()

  end

  let state events (query:Uri.Query.t) () =
    match Api.Query.Time.get' query with
    | Ok (None,_)       -> state_now events ()
    | Ok (Some t,query) -> Archive.state t query ()
    | Error e           -> respond_bad_query e

  let status events (query:Uri.Query.t) () =
    match Api.Query.Time.get' query with
    | Ok (None,_)       -> respond_error ~status:`Not_implemented "not implemented" ()
    | Ok (Some t,query) -> Archive.status t query ()
    | Error e           -> respond_bad_query e

  let errors events (query:Uri.Query.t) () =
    match Api.Query.Time.get' query with
    | Ok (None,_)       -> respond_error ~status:`Not_implemented "not implemented" ()
    | Ok (Some t,query) -> Archive.errors t query ()
    | Error e           -> respond_bad_query e

end

let handler api events id meth ({path;query;_}:Uri.sep) sock_data headers body =
  let is_guest = Common.User.eq id `Guest in
  match Api.Headers.is_ws headers,meth,path with
  (* WS *)
  | true, `GET, ["mode";"t2mi"]   -> WS.mode `T2MI sock_data events body ()
  | true, `GET, ["mode";"jitter"] -> WS.mode `JITTER sock_data events body ()
  | true, `GET, ["state"]         -> WS.state sock_data events body ()
  | true, `GET, ["status"]        -> WS.status sock_data events body ()
  | true, `GET, ["errors"]        -> WS.errors sock_data events body ()
  (* HTTP *)
  | false,`POST,["mode";"t2mi"]   -> HTTP.post_mode `T2MI api body ()
  | false,`POST,["mode";"jitter"] -> HTTP.post_mode `JITTER api body ()
  | false,`GET, ["mode";"t2mi"]   -> HTTP.mode `T2MI api ()
  | false,`GET, ["mode";"jitter"] -> HTTP.mode `JITTER api ()
  | false,`POST,["port";id;state] ->
     let id'    = int_of_string_opt id in
     let state' = bool_of_string_opt state in
     (match id',state' with
      | Some id,Some state -> redirect_if is_guest @@ HTTP.post_port id state api
      | None, _            -> respond_error_other @@ Printf.sprintf "bad port id: %s" id
      | _, None            -> respond_error_other @@ Printf.sprintf "bad state: %s" state)
  | false,`GET, ["reset"]        -> redirect_if is_guest @@ HTTP.post_reset api
  | false,`GET, ["info"]         -> HTTP.devinfo api ()
  | false,`GET, ["state"]        -> HTTP.state events query ()
  | false,`GET, ["status"]       -> HTTP.status events query ()
  | false,`GET, ["errors"]       -> HTTP.errors events query ()
  | _ -> not_found ()
