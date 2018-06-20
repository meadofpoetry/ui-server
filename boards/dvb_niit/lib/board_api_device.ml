open Containers
open Common
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect

(** API
    POST /device/reset

    GET  /device/info
    GET  /device/config
    GET  /device/state
 *)

module WS = struct

  let state sock_data (events:events) body () =
    sock_handler sock_data (React.S.changes events.state) Topology.state_to_yojson body

end

module HTTP = struct

  let post_reset (api:api) () =
    api.reset () >|= Result.return
    >>= Json.respond_result_unit

  let devinfo (api:api) () =
    api.get_devinfo () >|= (devinfo_opt_to_yojson %> Result.return)
    >>= Json.respond_result

  let config (api:api) () =
    api.get_config () >|= (config_to_yojson %> Result.return)
    >>= Json.respond_result

  let state_now (events:events) () =
    React.S.value events.state
    |> Common.Topology.state_to_yojson
    |> Result.return
    |> Json.respond_result

  module Archive = struct

    let state time (query:Uri.Query.t) () =
      respond_error ~status:`Not_implemented "not_implemented" ()

  end

end

let handler api events id meth ({path;query;_}:Uri.sep) sock_data headers body =
  let is_guest = Common.User.eq id `Guest in
  match Api.Headers.is_ws headers,meth,path with
  (* WS *)
  | true, `GET, ["state"] -> WS.state sock_data events body ()
  (* HTTP *)
  | false,`POST,["reset"] -> redirect_if is_guest @@ HTTP.post_reset api
  | _ -> not_found ()
