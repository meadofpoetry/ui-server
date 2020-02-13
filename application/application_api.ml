open Application_types
open Lwt.Infix

module Event = struct
  let get_topology (app : Application.t) _user =
    let event = React.S.changes app.topo |> React.E.map Topology.to_yojson in
    Lwt.return event

  let get_streams (app : Application.t) _user =
    let event =
      React.S.changes app.hw.streams |> React.E.map Stream.stream_table_to_yojson
    in
    Lwt.return event

  let get_log (app : Application.t) inputs streams _user =
    match Application.log_for_input app inputs streams with
    | Error e -> Lwt.fail_with e
    | Ok event ->
        let event =
          React.E.map (Util_json.List.to_yojson Stream.Log_message.to_yojson) event
        in
        Lwt.return event
end

module Interval = Time.Interval.Make (Ptime_clock)

let set_streams (app : Application.t) _user body _env _state =
  match Stream.stream_setting_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok s -> (
      Hardware.set_stream app.hw s
      >>= function
      | Ok () -> Lwt.return `Unit
      | Error ejs ->
          (* TODO proper string *)
          Lwt.return
            (`Error (Yojson.Safe.to_string @@ Stream.Table.set_error_to_yojson ejs)))

let get_topology (app : Application.t) _user _body _env _state =
  app.topo |> React.S.value |> Topology.to_yojson |> fun v -> Lwt.return (`Value v)

let get_streams (app : Application.t) _user _body _env _state =
  app.hw.streams
  |> React.S.value
  |> Stream.stream_table_to_yojson
  |> fun v -> Lwt.return (`Value v)

let get_all_streams (app : Application.t) input _user _body _env _state =
  match Application.streams_on_input app input with
  | Ok v -> Lwt.return (`Value (Stream.stream_list_to_yojson v))
  | Error e -> Lwt.return (`Error e)

let get_stream_source (app : Application.t) id _user _body _env _state =
  match Application.stream_source app id with
  | Ok v -> Lwt.return (`Value (Stream.source_to_yojson v))
  | Error e -> Lwt.return (`Error e)

let get_log
    (app : Application.t)
    boards
    cpu
    inputs
    streams
    limit
    from
    till
    duration
    order
    _user
    _body
    _env
    _state =
  match Interval.make ?from ?till ?duration () with
  | Ok (`Range (from, till)) ->
      Database.Log.select
        app.db
        ~boards
        ~cpu
        ~inputs
        ~streams
        ?limit
        ?order
        ~from
        ~till
        ()
      |> Lwt.map
           (Api.rows_to_yojson
              (Util_json.List.to_yojson Stream.Log_message.to_yojson)
              (fun () -> `Null))
      >>= fun x -> Lwt.return (`Value x)
  | _ -> Lwt.return `Not_implemented
