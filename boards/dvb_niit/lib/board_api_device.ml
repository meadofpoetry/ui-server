open Containers
open Common
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect

module WS = struct

  let state (events:events) _ body sock_data () =
    sock_handler sock_data (React.S.changes events.state) Topology.state_to_yojson body

  let config (events:events) _ _ body sock_data () =
    sock_handler sock_data events.config config_to_yojson body

end

module HTTP = struct

  let post_reset (api:api) _ _ () =
    api.reset () >|= Result.return
    >>= Json.respond_result_unit

  let devinfo (api:api) _ _ () =
    api.get_devinfo () >|= (devinfo_opt_to_yojson %> Result.return)
    >>= Json.respond_result

  let config (api:api) _ _ () =
    api.get_config ()
    |> config_to_yojson
    |> Result.return
    |> Json.respond_result

  let state (events:events) _ _ () =
    React.S.value events.state
    |> Common.Topology.state_to_yojson
    |> Result.return
    |> Json.respond_result

  module Archive = struct

    let state from till duration _ _ () =
      let _ = Time.make_interval ?from ?till ?duration () in
      respond_error ~status:`Not_implemented "not_implemented" ()

  end

end

let handler api events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "device"
    [ create_ws_handler ~docstring:"Returns current board state"
                        ~path:Path.Format.("state" @/ empty)
                        ~query:Query.empty
                        (WS.state events)
    ; create_ws_handler ~docstring:"Returns current board configuration"
                        ~path:Path.Format.("config" @/ empty)
                        ~query:Query.(["name", (module Option(String))])
                        (WS.config events)
    ]
    [ `GET,  [ create_handler ~docstring:"Returns current board state"
                              ~path:Path.Format.("state" @/ empty)
                              ~query:Query.empty
                              (HTTP.state events)
             ; create_handler ~docstring:"Returns current board description, if available"
                              ~path:Path.Format.("info" @/ empty)
                              ~query:Query.empty
                              (HTTP.devinfo api)
             ; create_handler ~docstring:"Returns current board configuration"
                              ~path:Path.Format.("config" @/ empty)
                              ~query:Query.empty
                              (HTTP.config api)
             (* Archive *)
             ; create_handler ~docstring:"Returns board states for the requested period"
                              ~path:Path.Format.("state/archive" @/ empty)
                              ~query:Query.[ "from",    (module Option(Time.Show))
                                           ; "to",      (module Option(Time.Show))
                                           ; "duration",(module Option(Time.Relative)) ]
                              HTTP.Archive.state
             ]
    ; `POST, [ create_handler ~docstring:"Resets the board"
                              ~restrict:[ `Guest ]
                              ~path:Path.Format.("reset" @/ empty)
                              ~query:Query.empty
                              (HTTP.post_reset api)
             ]
    ]
