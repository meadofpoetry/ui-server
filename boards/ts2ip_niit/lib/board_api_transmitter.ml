open Containers
open Board_protocol
open Board_types
open Api.Redirect
open Api.Interaction
open Api.Interaction.Json
open Board_api_common
open Common

module WS = struct

  open Api.Socket

  let status (events:events) _ body sock_data () =
    handler socket_table sock_data events.status status_to_yojson body

  let mode (events:events) _ body sock_data () =
    let e = React.E.map (fun (x:config) -> x.packers) events.config in
    handler socket_table sock_data e (Json.List.to_yojson packer_settings_to_yojson) body

  let in_streams (events:events) _ body sock_data () =
    let e = React.S.changes events.in_streams in
    handler socket_table sock_data e (Json.List.to_yojson Stream.to_yojson) body

  let out_streams (events:events) _ body sock_data () =
    let e = React.S.changes events.out_streams in
    handler socket_table sock_data e (Json.List.to_yojson Stream.to_yojson) body

end

module HTTP = struct

  let set_mode (api:api) _ body () =
    of_body body >>= fun json ->
    (match (Json.List.of_yojson stream_settings_of_yojson) json with
     | Error e -> Lwt_result.fail @@ of_error_string e
     | Ok mode -> api.set_packers mode
                  |> Lwt_result.map_err packers_error_to_yojson)
    >>= respond_result_unit

  let set_streams (api:api) _ body () =
    of_body body >>= fun json ->
    (match (Json.List.of_yojson Stream.of_yojson) json with
     | Error e -> Lwt_result.fail @@ of_error_string e
     | Ok s    -> api.set_streams s
                  |> Lwt_result.map_err packers_error_to_yojson)
    >>= respond_result_unit

  let get_mode (api:api) _ body () =
    (api.config ()).packers |> Json.List.to_yojson packer_settings_to_yojson
    |> Result.return |> respond_result

  let get_in_streams (api:api) _ body () =
    api.in_streams () |> Json.List.to_yojson Stream.to_yojson
    |> Result.return |> respond_result

  let get_out_streams (api:api) _ body () =
    api.out_streams () |> Json.List.to_yojson Stream.to_yojson
    |> Result.return |> respond_result

  let get_status (api:api) _ body () =
    api.status () |> Json.Option.to_yojson status_to_yojson
    |> Result.return |> respond_result

end

let handler api events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "transmitter"
    [ create_ws_handler ~docstring:"Notifies client when board mode is changed"
        ~path:Path.Format.("mode" @/ empty)
        ~query:Query.empty
        (WS.mode events)
    ; create_ws_handler ~docstring:"Pushes board status to the client"
        ~path:Path.Format.("status" @/ empty)
        ~query:Query.empty
        (WS.status events)
    ; create_ws_handler ~docstring:"Pushes input streams to the client"
        ~path:Path.Format.("streams/input" @/ empty)
        ~query:Query.empty
        (WS.in_streams events)
    ; create_ws_handler ~docstring:"Pushes output streams to the client"
        ~path:Path.Format.("streams/output" @/ empty)
        ~query:Query.empty
        (WS.out_streams events)
    ]
    [ `POST, [ create_handler ~docstring:"Sets transmitter mode"
                 ~path:Path.Format.("mode" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_mode api)
             ; create_handler ~docstring:"Sets streams to transmit"
                 ~path:Path.Format.("streams" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_streams api)
             ]
    ; `GET,  [ create_handler ~docstring:"Returns current transmitter mode"
                 ~path:Path.Format.("mode" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_mode api)
             ; create_handler ~docstring:"Returns latest board status to the client, if any"
                 ~path:Path.Format.("status" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_status api)
             ; create_handler ~docstring:"Returns input streams to the client"
                 ~path:Path.Format.("streams/input" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_in_streams api)
             ; create_handler ~docstring:"Returns output streams to the client"
                 ~path:Path.Format.("streams/output" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_out_streams api)
             ]
    ]
