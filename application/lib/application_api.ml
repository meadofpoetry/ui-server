open Containers
open Api.Interaction
open Api.Interaction.Json
open Common.Topology
open Boards.Board
open Application_types

let socket_table = Hashtbl.create 1000

module WS = struct

  let get_topology (app:Application.t) _ body sock_data () =
    Api.Socket.handler socket_table sock_data (React.S.changes app.topo) to_yojson body

  let get_streams (app:Application.t) _ body sock_data () =
    Api.Socket.handler socket_table sock_data (React.S.changes app.hw.streams) stream_table_to_yojson body

end

module HTTP = struct

  let set_streams (app:Application.t) _ body () =
    of_body body >>= fun js ->
    match stream_setting_of_yojson js with
    | Error e -> respond_error e ()
    | Ok s    -> Hardware.set_stream app.hw s >>= function
                 | Ok () as r -> Json.respond_result_unit r
                 | Error ejs  -> Json.respond_result_unit (Error (set_error_to_yojson ejs))

  let get_topology (app:Application.t) _ _ () =
    app.topo |> React.S.value |> to_yojson |> Result.return |> respond_result

  let get_streams (app:Application.t) _ _ () =
    app.hw.streams |> React.S.value |> stream_table_to_yojson |> Result.return |> respond_result

end

let topo_handler (app:Application.t) =
  let open Common.Uri in
  let open Api_handler in
  create_dispatcher
    "topology"
    [ create_ws_handler ~docstring:"Pushes device topology to the client"
        ~path:Path.Format.empty
        ~query:Query.empty
        (WS.get_topology app)
    ; create_ws_handler ~docstring:"Pushes stream table to the client"
        ~path:Path.Format.("streams" @/ empty)
        ~query:Query.empty
        (WS.get_streams app)
    ]
    [ `POST, [ create_handler ~docstring:"Sets streams that are received by PC process"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("streams" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_streams app)
             ]
    ; `GET,  [ create_handler ~docstring:"Returns device topology"
                 ~path:Path.Format.empty
                 ~query:Query.empty
                 (HTTP.get_topology app)
             ; create_handler ~docstring:"Returns stream table"
                 ~path:Path.Format.("streams" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_streams app)
             ]
    ]

let handlers (app:Application.t) =
  let board_api = Hardware.Map.fold (fun _ x acc -> x.handlers @ acc) app.hw.boards [] in
  let proc_api  = match app.proc with
    | None      -> []
    | Some proc -> proc#handlers () (* TODO ? *)
  in
  [ Api_handler.add_layer "board" board_api
  ; topo_handler app
  ; Pc_control.Network_api.handlers app.network
  ; User_api.handlers app.users
  ]
  @ proc_api
