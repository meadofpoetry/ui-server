open Containers
open Qoe_errors
open Pipeline_protocol
open Pipeline_api_common
open Api.Interaction
open Api.Interaction.Json
open Common

module WS = struct

  open React

  let get_settings (api : api) _ body sock_data () =
    let event = S.changes api.notifs.settings in
    Api.Socket.handler socket_table sock_data event
      Settings.to_yojson body

  let get_wm (api : api) _ body sock_data () =
    let event = S.changes api.notifs.wm in
    Api.Socket.handler socket_table sock_data event
      Wm.to_yojson body

  let get_status (api : api) _ body sock_data () =
    let event = S.changes api.notifs.status in
    Api.Socket.handler socket_table sock_data event
      Qoe_status.status_list_to_yojson body

end

module HTTP = struct

  let set body conv apply =
    of_body body >>= fun js ->
    match conv js with
    | Error e -> respond_error e ()
    | Ok x -> apply x >>= function Ok () -> respond_result_unit (Ok ())

  let set_settings (api : api) headers body () =
    set body Settings.of_yojson
      Pipeline_protocol.(fun x -> api.requests.settings.set x)

  let set_wm (api : api) _ body () =
    set body Wm.of_yojson
      Pipeline_protocol.(fun x -> api.requests.wm.set x)

  let get_settings (api : api) headers body () =
    api.requests.settings.get ()
    >|= (function
         | Error e -> Error (Json.String.to_yojson e)
         | Ok v -> Ok (Settings.to_yojson v))
    >>= respond_result

  let get_wm (api : api) _ body () =
    api.requests.wm.get ()
    >|= (function
         | Error e -> Error (Json.String.to_yojson e)
         | Ok v -> Ok (Wm.to_yojson v))
    >>= respond_result

  let get_status (api : api) _ body () =
    React.S.value api.notifs.status
    |> Qoe_status.status_list_to_yojson
    |> fun r -> respond_result (Ok r)

end

let settings_handler (api : api) =
  let open Api_handler in
  create_dispatcher "settings"
    [ create_ws_handler ~docstring:"Settings socket"
        ~path:Uri.Path.Format.empty
        ~query:Uri.Query.empty
        (WS.get_settings api)
    ]
    [ `GET, [ create_handler ~docstring:"Settings"
                ~path:Uri.Path.Format.empty
                ~query:Uri.Query.empty
                (HTTP.get_settings api)
            ]
    ; `POST, [ create_handler ~docstring:"Post settings"
                 ~restrict:[`Guest]
                 ~path:Uri.Path.Format.empty
                 ~query:Uri.Query.empty
                 (HTTP.set_settings api)
             ]
    ]

let wm_handler (api : api) =
  let open Api_handler in
  create_dispatcher "wm"
    [ create_ws_handler ~docstring:"WM socket"
        ~path:Uri.Path.Format.empty
        ~query:Uri.Query.empty
        (WS.get_wm api)
    ]
    [ `GET, [ create_handler ~docstring:"Wm"
                ~path:Uri.Path.Format.empty
                ~query:Uri.Query.empty
                (HTTP.get_wm api)
            ]
    ; `POST, [ create_handler ~docstring:"Post wm"
                 ~restrict:[`Guest]
                 ~path:Uri.Path.Format.empty
                 ~query:Uri.Query.empty
                 (HTTP.set_wm api)
             ]
    ]

let status_handler (api : api) =
  let open Api_handler in
  create_dispatcher
    "status"
    [ create_ws_handler ~docstring:"Stream status socket"
        ~path:Uri.Path.Format.empty
        ~query:Uri.Query.empty
        (WS.get_status api)
    ]
    [ `GET,  [ create_handler ~docstring:"Status"
                 ~path:Uri.Path.Format.empty
                 ~query:Uri.Query.empty
                 (HTTP.get_status api)
             ]
    ]

let handlers (api : api) =
  [ Api_handler.add_layer "pipeline"
      [ settings_handler api
      ; wm_handler api
      ; status_handler api
      ; Pipeline_api_structures.handler api
      ; Pipeline_api_measurements.handler api
      ; Pipeline_api_history.handler api
      ]
  ]
