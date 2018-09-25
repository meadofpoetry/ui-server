open Containers
open Board_protocol
open Board_types
open Api.Interaction.Json
open Board_api_common
open Common

module WS = struct

  open Api.Socket

  let state (events:events) _ body sock_data () =
    handler socket_table sock_data (React.S.changes events.state) Topology.state_to_yojson body

  let mode (events:events) _ body sock_data () =
    let e = React.E.map (fun (x:config) -> x.nw_mode) events.config in
    handler socket_table sock_data e nw_settings_to_yojson body

end

module HTTP = struct

  let set_mode (api:api) _ body () =
    of_body body >>= fun mode ->
    (match nw_settings_of_yojson mode with
     | Error e -> Lwt_result.fail @@ of_error_string e
     | Ok mode -> api.set_nw_mode mode >|= Result.return)
    >>= respond_result_unit

  let get_state (events:events) _ _ () =
    React.S.value events.state |> Topology.state_to_yojson
    |> Result.return |> respond_result

  let get_devinfo (api:api) _ _ () =
    api.devinfo () |> Json.Option.to_yojson devinfo_to_yojson
    |> Result.return |> respond_result

  let get_mac (api:api) _ _ () =
    (api.config ()).factory_mode.mac |> Macaddr.to_yojson
    |> Result.return |> respond_result

  let get_mode (api:api) _ _ () =
    (api.config ()).nw_mode |> nw_settings_to_yojson
    |> Result.return |> respond_result

  module Archive = struct

    (* let get_state limit compress from till duration _ _ () =
     *   not_found () *)

  end

end

let handler api events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "device"
    [ create_ws_handler ~docstring:"Notifies client when board state changes"
        ~path:Path.Format.("state" @/ empty)
        ~query:Query.empty
        (WS.state events)
    ; create_ws_handler ~docstring:"Notifies client when board mode is changed"
        ~path:Path.Format.("mode" @/ empty)
        ~query:Query.empty
        (WS.mode events)
    ]
    [ `POST, [ create_handler ~docstring:"Changes board network settings"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("mode" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_mode api)
             ]
    ; `GET,  [ create_handler ~docstring:"Returns current board state"
                 ~path:Path.Format.("state" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_state events)
             ; create_handler ~docstring:"Retunrs board description"
                 ~path:Path.Format.("info" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_devinfo api)
             ; create_handler ~docstring:"Returns board MAC address"
                 ~path:Path.Format.("mac" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_mac api)
             ; create_handler ~docstring:"Returns current board mode"
                 ~path:Path.Format.("mode" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_mode api)
             (* Archive *)
             (* ; create_handler ~docstring:"Returns archived board state"
              *     ~path:Path.Format.("state/archive" @/ empty)
              *     ~query:Query.[ "limit",    (module Option(Int))
              *                  ; "compress", (module Option(Bool))
              *                  ; "from",     (module Option(Time.Show))
              *                  ; "to",       (module Option(Time.Show))
              *                  ; "duration", (module Option(Time.Relative)) ]
              *     (HTTP.Archive.get_state) *)
             ]
    ]
