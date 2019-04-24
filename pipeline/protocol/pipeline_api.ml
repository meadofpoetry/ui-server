open Pipeline_types
open Lwt.Infix

module Event = struct

  open Util_react
  (*
let get_settings (api : api) _ body sock_data () =
    let event = S.changes api.notifs.settings in
    Api.Socket.handler socket_table sock_data event
      Settings.to_yojson body
   *)

  let get_wm_layout (api : Protocol.api) _user _body _env state =
    let event =
      S.changes api.notifs.wm
      |> E.map Wm.to_yojson
    in
    Lwt.return (`Ev (state, event))

  let get_status (api : Protocol.api) ids _user _body _env state =
    let event = S.changes api.notifs.status in
    let event = match ids with
      | [] -> event
      | ids ->
         React.E.map (List.filter (fun (x : Qoe_status.t) ->
                          List.exists (Application_types.Stream.ID.equal x.stream) ids))
           event
    in
    let event = E.map (Util_json.List.to_yojson Qoe_status.to_yojson) event in
    Lwt.return (`Ev (state, event))

end
             
let set body conv apply =
  match conv body with
  | Error e -> Lwt.return (`Error e)
  | Ok x ->
     apply x
     >>= function Ok () -> Lwt.return `Unit
                | Error (`Qoe_backend e) -> Lwt.return (`Error e) (* TODO respond result *)
(*
  let set_settings (api : api) headers body () =
    set body Settings.of_yojson
      Pipeline_protocol.(fun x -> api.requests.settings.set x)

  let get_settings (api : api) headers body () =
    api.requests.settings.get ()
    >|= (function
         | Error e -> Error (Json.String.to_yojson e)
         | Ok v -> Ok (Settings.to_yojson v))
    >>= respond_result
 *)
let apply_wm_layout (state : Protocol.state) (api : Protocol.api) _user body _env _state =
  let (>>=) = Lwt_result.bind in
  set body Wm.of_yojson
    Protocol.(fun x ->
    match state.backend with
    | None -> Lwt.return_error (`Qoe_backend "not ready")
    | Some backend ->
       Qoe_backend.Mosaic.apply_layout backend x
       >>= fun () ->
       Lwt_result.ok @@ api.options.wm#set x)

let get_wm_layout (state : Protocol.state) (api : Protocol.api) _user _body _env _state =
  match state.backend with
  | None -> Lwt.return (`Error "not ready")
  | Some backend ->
     Protocol.Qoe_backend.Mosaic.get_layout backend
     >>= (function
          | Error (`Qoe_backend e) -> Lwt.return (`Error e)
          | Ok v -> Lwt.return (`Value (Wm.to_yojson v)))

let get_status (api : Protocol.api) ids _user _body _env _state =
  React.S.value api.notifs.status
  |> (fun l ->
    match ids with
    | [] -> l
    | ids -> List.filter (fun (x : Qoe_status.t) ->
                 List.exists (Application_types.Stream.ID.equal x.stream) ids) l)
  |> Qoe_status.status_list_to_yojson
  |> fun r -> Lwt.return (`Value r)
                 

(*


let handlers (api : api) =
  [ Api_handler.add_layer "pipeline"
      ; Pipeline_api_measurements.handler api
      ; Pipeline_api_history.handler api
      ]
  ]
 *)
