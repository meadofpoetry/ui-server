open Containers
open Api.Interaction
open Api.Interaction.Json
open Common.Topology
open Boards.Board
open Application_types
open Common

let socket_table = Hashtbl.create 1000

module WS = struct

  let get_topology (app : Application.t) _ body sock_data () =
    Api.Socket.handler socket_table sock_data (React.S.changes app.topo) to_yojson body

  let get_streams (app : Application.t) _ body sock_data () =
    Api.Socket.handler socket_table sock_data (React.S.changes app.hw.streams) stream_table_to_yojson body

  let get_log (app : Application.t) inputs streams _ body sock_data () =
    match Application.log_for_input app inputs streams with
    | Error e  -> respond_not_found ()
    | Ok event ->
       Api.Socket.handler
         socket_table sock_data
         event (Json.List.to_yojson Stream.Log_message.to_yojson)
         body
    
end

module HTTP = struct

  let set_streams (app : Application.t) _ body () =
    of_body body >>= fun js ->
    match stream_setting_of_yojson js with
    | Error e -> respond_error e ()
    | Ok s ->
       Hardware.set_stream app.hw s >>= function
       | Ok () as r -> respond_result_unit r
       | Error ejs -> respond_result_unit (Error (Stream.Table.set_error_to_yojson ejs))

  let get_topology (app : Application.t) _ _ () =
    app.topo |> React.S.value |> to_yojson |> Result.return |> respond_result

  let get_streams (app : Application.t) _ _ () =
    app.hw.streams |> React.S.value |> stream_table_to_yojson |> Result.return |> respond_result

  let get_all_streams (app : Application.t) input _ _ () =
    Application.streams_on_input app input
    |> Result.map2 Application_types.stream_list_to_yojson (fun e -> `String e)
    |> respond_result

  let get_stream_source (app : Application.t) id _ _ () =
    let open Common.Stream in
    Application.stream_source app id
    |> Result.map2 source_to_yojson (fun e -> `String e)
    |> respond_result

  let get_log (app : Application.t) boards cpu inputs streams
        limit from till duration _ _ () =
    match Time.make_interval ?from ?till ?duration () with
    | Ok `Range (from, till) ->
       Database.Log.select app.db ~boards ~cpu ~inputs ~streams
         ?limit ~from ~till ()
       |> Lwt.map (Api.Api_types.rows_to_yojson
                     (Json.List.to_yojson Stream.Log_message.to_yojson)
                     (fun () -> `Null))
       >>= fun x -> Lwt_result.return x
       >>= respond_result
    | _ -> respond_error ~status:`Not_implemented "FIXME" ()

end

let topo_handler (app : Application.t) =
  let open Common.Uri in
  let open Api_handler in
  create_dispatcher
    "topology"
    [ create_ws_handler ~docstring:"Pushes device topology to the client"
        ~path:Path.Format.empty
        ~query:Query.empty
        (WS.get_topology app)
    ; create_ws_handler ~docstring:"Pushes stream table to the client"
        ~path:Path.Format.("stream_table" @/ empty)
        ~query:Query.empty
        (WS.get_streams app)
    ; create_ws_handler ~docstring:"Log for input (and stream)"
        ~path:Path.Format.("log" @/ empty)
        ~query:Query.["input", (module List(Show_topo_input));
                      "id", (module List(Stream.ID))]
        (WS.get_log app)
    ]
    [ `POST, [ create_handler ~docstring:"Sets streams that are received by PC process"
                 ~restrict:[`Guest]
                 ~path:Path.Format.("stream_table" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_streams app)
             ]
    ; `GET,  [ create_handler ~docstring:"Returns device topology"
                 ~path:Path.Format.empty
                 ~query:Query.empty
                 (HTTP.get_topology app)
             ; create_handler ~docstring:"Returns stream table"
                 ~path:Path.Format.("stream_table" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_streams app)
             ; create_handler ~docstring:"Returns all streams"
                 ~path:Path.Format.("streams" @/ empty)
                 ~query:Query.["input", (module Single(Show_topo_input))]
                 (HTTP.get_all_streams app)
             ; create_handler ~docstring:"Returns the source of a last suitable stream"
                 ~path:Path.Format.("source" @/ empty)
                 ~query:Query.["id", (module Single(Stream.ID))]
                 (HTTP.get_stream_source app)
             ; create_handler ~docstring:"Log for input (and stream)"
                 ~path:Path.Format.("log" @/ empty)
                 ~query:Query.[ "board", (module List(Int))
                              ; "cpu", (module List(String))
                              ; "input", (module List(Show_topo_input))
                              ; "id", (module List(Stream.ID))
                              ; "limit", (module Option(Int))
                              ; "from", (module Option(Time.Show))
                              ; "to", (module Option(Time.Show))
                              ; "duration", (module Option(Time.Relative)) ]
                 (HTTP.get_log app)
             ]
    ]

let handlers (app:Application.t) =
  let board_api = Hardware.Map.fold (fun _ x acc -> x.handlers @ acc) app.hw.boards [] in
  let proc_api  = match app.proc with
    | None -> []
    | Some proc -> proc#handlers () (* TODO ? *)
  in
  [ Api_handler.add_layer "board" board_api
  ; topo_handler app
  ; Pc_control.Network_api.handlers app.network
  ; User_api.handlers app.users
  ]
  @ proc_api
