open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Api.Redirect
open Common

type events = device_events

module WS = struct

  let state (events:events) _ body sock_data () =
    Api.Socket.handler socket_table sock_data (React.S.changes events.state)
      Common.Topology.state_to_yojson body

  let status (events:events) _ body sock_data () =
    Api.Socket.handler socket_table sock_data events.status status_to_yojson body

  let errors (events:events) errors _ body sock_data () =
    let e = match errors with
      | [] -> events.errors
      | l  -> React.E.fmap (fun l ->
                  List.filter (fun (x:board_error) -> List.mem ~eq:(=) x.err_code errors) l
                  |> function [] -> None | l -> Some l) events.errors
    in Api.Socket.handler socket_table sock_data e board_errors_to_yojson body

  let mode mode (events:events) _ body sock_data () =
    let f = fun e conv -> Api.Socket.handler socket_table sock_data e conv body in
    (match mode with
     | `T2MI   -> let e = React.E.map (fun x -> x.t2mi_mode) events.config
                          |> React.E.changes ~eq:(Equal.option equal_t2mi_mode)
                  in f e (Json.Option.to_yojson t2mi_mode_to_yojson)
     | `JITTER -> let e = React.E.map (fun x -> x.jitter_mode) events.config
                          |> React.E.changes ~eq:(Equal.option equal_jitter_mode)
                  in f e (Json.Option.to_yojson jitter_mode_to_yojson))

end

module HTTP = struct

  let post_reset (api:api) _ _ () =
    api.reset () >|= Result.return
    >>= respond_result_unit

  let post_t2mi_mode (api:api) _ body () =
    of_body body >>= fun mode ->
    (match (Json.Option.of_yojson t2mi_mode_of_yojson) mode with
     | Error e -> Lwt_result.fail @@ of_error_string e
     | Ok mode -> api.set_t2mi_mode mode >|= Result.return)
    >>= respond_result_unit

  let post_jitter_mode (api:api) _ body () =
    of_body body >>= fun mode ->
    (match (Json.Option.of_yojson jitter_mode_of_yojson) mode with
     | Error e -> Lwt_result.fail @@ of_error_string e
     | Ok mode -> api.set_jitter_mode mode >|= Result.return)
    >>= respond_result_unit

  let post_port (api:api) port en _ _ () = match Board_parser.input_of_int port, en with
    | Some i,   true  -> api.set_input i   >|= Result.return >>= respond_result_unit
    | Some ASI, false -> api.set_input SPI >|= Result.return >>= respond_result_unit
    | Some SPI, false -> api.set_input ASI >|= Result.return >>= respond_result_unit
    | _               -> not_found ()

  let state (events:events) _ _ () =
    React.S.value events.state
    |> Common.Topology.state_to_yojson
    |> Result.return
    |> respond_result

  let devinfo api _ _ () =
    api.get_devinfo () >|= (Json.Option.to_yojson devinfo_to_yojson %> Result.return)
    >>= respond_result

  let mode mode (api:api) _ _ () =
    let open Json.Option in
    (match mode with
     | `T2MI   -> api.config () >|= (fun x -> Ok ((to_yojson t2mi_mode_to_yojson) x.t2mi_mode))
     | `JITTER -> api.config () >|= (fun x -> Ok ((to_yojson jitter_mode_to_yojson x.jitter_mode))))
    >>= respond_result

  (** Archive GET requests **)
  module Archive = struct

    let state limit compress from till duration _ _ () =
      respond_error ~status:`Not_implemented "not impelemented" ()

    let errors errors limit compress from till duration _ _ () =
      respond_error ~status:`Not_implemented "not impelemented" ()

  end

end

let handler api events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "device"
    [ create_ws_handler ~docstring:"Pushes board state to the client when changes"
        ~path:Path.Format.("state" @/ empty)
        ~query:Query.empty
        (WS.state events)
    ; create_ws_handler ~docstring:"Pushes T2-MI analysis mode to the client when changes"
        ~path:Path.Format.("mode/t2mi" @/ empty)
        ~query:Query.empty
        (WS.mode `T2MI events)
    ; create_ws_handler ~docstring:"Pushes jitter measure mode to the client when changes"
        ~path:Path.Format.("mode/jitter" @/ empty)
        ~query:Query.empty
        (WS.mode `JITTER events)
    ; create_ws_handler ~docstring:"Pushes board status to the client"
        ~path:Path.Format.("status" @/ empty)
        ~query:Query.empty
        (WS.status events)
    ; create_ws_handler ~docstring:"Pushes board errors to the client if any"
        ~path:Path.Format.("errors" @/ empty)
        ~query:Query.["errors", (module List(Int))]
        (WS.errors events)
    ]
    [ `POST, [ create_handler ~docstring:"Sets T2-MI analysis mode"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("mode/t2mi" @/ empty)
                 ~query:Query.empty
                 (HTTP.post_t2mi_mode api)
             ; create_handler ~docstring:"Sets jitter measure mode"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("mode/jitter" @/ empty)
                 ~query:Query.empty
                 (HTTP.post_jitter_mode api)
             ; create_handler ~docstring:"Switches board input to receive a stream from"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("port" @/ Int ^/ Bool ^/ empty)
                 ~query:Query.empty
                 (HTTP.post_port api)
             ; create_handler ~docstring:"Resets the board"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("reset" @/ empty)
                 ~query:Query.empty
                 (HTTP.post_reset api)
             ]
    ; `GET,  [ create_handler ~docstring:"Returns current T2-MI analysis mode"
                 ~path:Path.Format.("mode/t2mi" @/ empty)
                 ~query:Query.empty
                 (HTTP.mode `T2MI api)
             ; create_handler ~docstring:"Returns current jitter measure mode"
                 ~path:Path.Format.("mode/jitter" @/ empty)
                 ~query:Query.empty
                 (HTTP.mode `T2MI api)
             ; create_handler ~docstring:"Returns current board description, if available"
                 ~path:Path.Format.("info" @/ empty)
                 ~query:Query.empty
                 (HTTP.devinfo api)
             ; create_handler ~docstring:"Returns current board state"
                 ~path:Path.Format.("state" @/ empty)
                 ~query:Query.empty
                 (HTTP.state events)
             (* Archive *)
             ; create_handler ~docstring:"Returns archived board state"
                 ~path:Path.Format.("state/archive" @/ empty)
                 ~query:Query.[ "limit",    (module Option(Int))
                              ; "compress", (module Option(Bool))
                              ; "from",     (module Option(Time.Show))
                              ; "to",       (module Option(Time.Show))
                              ; "duration", (module Option(Time.Relative)) ]
                 HTTP.Archive.state
             ; create_handler ~docstring:"Returns archived board errors"
                 ~path:Path.Format.("errors/archive" @/ empty)
                 ~query:Query.[ "errors",   (module List(Int))
                              ; "limit",    (module Option(Int))
                              ; "compress", (module Option(Bool))
                              ; "from",     (module Option(Time.Show))
                              ; "to",       (module Option(Time.Show))
                              ; "duration", (module Option(Time.Relative)) ]
                 HTTP.Archive.errors
             ]
    ]
