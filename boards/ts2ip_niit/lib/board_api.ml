open Containers
open Board_protocol
open Board_types
open Api.Interaction
open Api.Interaction.Json
open Board_api_common
open Common

module WS = struct

  open Api.Socket

  let state s_state _ body sock_data =
    handler socket_table sock_data (Lwt_react.S.changes s_state) Topology.state_to_yojson body

  let status (events:events) _ body sock_data =
    handler socket_table sock_data events.status status_to_yojson body

  let config (events:events) _ body sock_data =
    handler socket_table sock_data events.config config_to_yojson body

  let in_streams s_streams _ body sock_data =
    handler socket_table sock_data (Lwt_react.S.changes s_streams) (Json.List.to_yojson Stream.to_yojson) body

end

module HTTP = struct

  let devinfo (api:api) () =
    api.devinfo () |> Json.Option.to_yojson devinfo_to_yojson
    |> Result.return |> respond_result

  let state s_state =
    React.S.value s_state |> Topology.state_to_yojson
    |> Result.return |> respond_result

  let config (api:api) ()=
    api.config () |> config_to_yojson
    |> Result.return |> respond_result

  let streams s_streams =
    React.S.value s_streams |> Json.List.to_yojson Stream.to_yojson
    |> Result.return |> respond_result

  let set_nw_mode (api:api) body () =
    of_body body >>= fun mode ->
    (match nw_settings_of_yojson mode with
     | Error e -> Lwt_result.fail @@ of_error_string e
     | Ok mode -> api.set_mode mode >|= Result.return)
    >>= respond_result_unit

  let set_streams_simple (api:api) body () =
    of_body body >>= fun sms ->
    (match Json.List.of_yojson Stream.of_yojson sms with
     | Error e -> Lwt_result.fail @@ of_error_string e
     | Ok sms  -> api.set_streams_simple sms
                  |> Lwt_result.map_err (fun e ->
                         of_error_string (Board_protocol.set_streams_error_to_string e)))
    >>= respond_result_unit

  let set_streams_full (api:api) body () =
    of_body body >>= fun sms ->
    (match (Json.List.of_yojson stream_settings_of_yojson) sms with
     | Error e -> Lwt_result.fail @@ of_error_string e
     | Ok sms  -> api.set_streams_full sms
                  |> Lwt_result.map_err (fun e ->
                         of_error_string (Board_protocol.set_streams_error_to_string e)))
    >>= respond_result_unit

end


(* let handle api events s_state s_streams (uri:Common.Uri.uri) _ meth _ body sock_data =
 *   let open Api.Redirect in
 *   (\* TODO match string + query *\)
 *   match meth, uri.path with
 *   | `POST, ["factory_mode"]   -> set_factory_mode api body ()
 *   | `POST, ["nw_mode"]        -> set_nw_mode api body ()
 *   | `POST, ["streams_simple"] -> set_streams_simple api body ()
 *   | `POST, ["streams_full"]   -> set_streams_full api body ()
 * 
 *   | `GET,  ["devinfo"]        -> devinfo api ()
 *   | `GET,  ["state"]          -> state s_state
 *   | `GET,  ["config"]         -> config api ()
 *   | `GET,  ["streams"]        -> streams s_streams
 * 
 *   | `GET,  ["state_ws"]       -> state_ws sock_data s_state body
 *   | `GET,  ["status_ws"]      -> status_ws sock_data events body
 *   | `GET,  ["config_ws"]      -> config_ws sock_data events body
 *   | `GET,  ["streams_ws"]     -> streams_ws sock_data s_streams body
 *   | _ -> not_found () *)

let handlers id api events =
  [ (* (module struct
     *    let domain = Common.Topology.get_api_path id
     *    let handle = handle api events s_state s_streams
     *  end : Api_handler.HANDLER) *) ]
