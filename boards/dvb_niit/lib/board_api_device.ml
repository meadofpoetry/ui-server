open Containers
open Board_types.Device
open Board_protocol
open Board_api_common
open Api.Interaction.Json
open Common

module WS = struct

  open Api.Socket
  open React

  let get_state (events : events) _ body sock_data () =
    handler socket_table sock_data
      (S.changes events.state) Topology.state_to_yojson body

  let get_receivers (events : events) _ body sock_data () =
    let to_yojson = Json.(Option.to_yojson @@ List.to_yojson Int.to_yojson) in
    let e =
      S.map ~eq:(Equal.option @@ Equal.list Int.equal) (function
          | None -> None
          | Some x -> Some x.receivers) events.devinfo
      |> S.changes in
    handler socket_table sock_data e to_yojson body

  let get_mode (events : events) ids _ body sock_data () =
    let e = match ids with
      | [] -> events.config
      | ids ->
         React.E.fmap (fun l ->
             List.filter (fun (id, _) -> List.mem ~eq:(=) id ids) l
             |> function [] -> None | l -> Some l) events.config in
    handler socket_table sock_data e config_to_yojson body

end

module HTTP = struct

  let reset (api : api) _ _ () =
    api.reset () >|= Result.return
    >>= respond_result_unit

  let set_mode (api : api) id _ body () =
    let to_yojson = Json.(Pair.to_yojson Int.to_yojson mode_rsp_to_yojson) in
    of_body body >>= fun mode ->
    (match mode_of_yojson mode with
     | Error e -> Lwt_result.fail @@ of_error_string e
     | Ok mode -> api.set_mode (id, mode) >|= (Result.return % to_yojson))
    >>= respond_result

  let get_devinfo (api : api) _ _ () =
    api.get_devinfo ()
    >|= (Result.return % Json.Option.to_yojson devinfo_to_yojson)
    >>= respond_result

  let get_receivers (api : api) _ _ () =
    api.get_devinfo ()
    >|= (function None -> None | Some x -> Some x.receivers)
    >|= Json.(Option.to_yojson @@ List.to_yojson Int.to_yojson)
    >|= Result.return
    >>= respond_result

  let get_mode (api : api) ids _ _ () =
    api.get_config ~ids ()
    >|= (Result.return % config_to_yojson)
    >>= respond_result

  let get_state (events : events) _ _ () =
    React.S.value events.state
    |> Topology.state_to_yojson
    |> Result.return
    |> respond_result

end

let handler api events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "device"
    [ create_ws_handler ~docstring:"Returns current board state"
        ~path:Path.Format.("state" @/ empty)
        ~query:Query.empty
        (WS.get_state events)
    ; create_ws_handler ~docstring:"Returns current board mode"
        ~path:Path.Format.("mode" @/ empty)
        ~query:Query.(["id", (module List(Int))])
        (WS.get_mode events)
    ; create_ws_handler ~docstring:"Returns available modules"
        ~path:Path.Format.("receivers" @/ empty)
        ~query:Query.empty
        (WS.get_receivers events)
    ]
    [ `POST,
      [ create_handler ~docstring:"Resets the board"
          ~restrict:[`Guest]
          ~path:Path.Format.("reset" @/ empty)
          ~query:Query.empty
          (HTTP.reset api)
      ; create_handler ~docstring:"Sets board mode"
          ~restrict:[`Guest]
          ~path:Path.Format.("mode" @/ Int ^/ empty)
          ~query:Query.empty
          (HTTP.set_mode api)
      ]
    ; `GET,
      [ create_handler ~docstring:"Returns current board state"
          ~path:Path.Format.("state" @/ empty)
          ~query:Query.empty
          (HTTP.get_state events)
      ; create_handler ~docstring:"Returns current board description, if available"
          ~path:Path.Format.("info" @/ empty)
          ~query:Query.empty
          (HTTP.get_devinfo api)
      ; create_handler ~docstring:"Returns available modules"
          ~path:Path.Format.("receivers" @/ empty)
          ~query:Query.empty
          (HTTP.get_receivers api)
      ; create_handler ~docstring:"Returns current board mode"
          ~path:Path.Format.("mode" @/ empty)
          ~query:Query.["id", (module List(Int))]
          (HTTP.get_mode api)
      ]
    ]
