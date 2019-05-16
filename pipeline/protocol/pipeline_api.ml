open Pipeline_types
open Lwt.Infix

module Event = struct

  open Protocol
  open Util_react
  (*
let get_settings (state : state) _ body sock_data () =
    let event = S.changes api.notifs.settings in
    Api.Socket.handler socket_table sock_data event
      Settings.to_yojson body
   *)

  let get_wm_layout (state : Protocol.state) _user _body _env _state =
    let event =
      S.changes state.notifs.wm
      |> E.map Wm.to_yojson
    in
    Lwt.return (`Ev event)

  let get_status (state : Protocol.state) ids _user _body _env _state =
    let event = S.changes state.notifs.status in
    let event = match ids with
      | [] -> event
      | ids ->
         React.E.map (List.filter (fun (x : Qoe_status.t) ->
                          List.exists (Application_types.Stream.ID.equal x.stream) ids))
           event
    in
    let event = E.map (Util_json.List.to_yojson Qoe_status.to_yojson) event in
    Lwt.return (`Ev event)

end

let set body conv apply =
  match conv body with
  | Error e -> Lwt.return (`Error e)
  | Ok x ->
     apply x
     >>= function Ok () -> Lwt.return `Unit
                | Error (`Qoe_backend e) -> Lwt.return (`Error e) (* TODO respond result *)
(*
  let set_settings (state : state) headers body () =
    set body Settings.of_yojson
      Pipeline_protocol.(fun x -> state.requests.settings.set x)

  let get_settings (state : state) headers body () =
    state.requests.settings.get ()
    >|= (function
         | Error e -> Error (Json.String.to_yojson e)
         | Ok v -> Ok (Settings.to_yojson v))
    >>= respond_result
 *)
let apply_wm_layout (state : Protocol.state) _user body _env _state =
  let (>>=) = Lwt_result.bind in
  set body Wm.of_yojson
    Protocol.(fun x ->
    match state.backend with
    | None -> Lwt.return_error (`Qoe_backend "not ready")
    | Some backend ->
       Qoe_backend.Mosaic.apply_layout backend x
       >>= fun () ->
       Lwt_result.ok @@ state.options.wm#set x)

let get_wm_layout (state : Protocol.state) _user _body _env _state =
  match state.backend with
  | None -> Lwt.return (`Error "not ready")
  | Some backend ->
     Protocol.Qoe_backend.Mosaic.get_layout backend
     >>= (function
          | Error (`Qoe_backend e) -> Lwt.return (`Error e)
          | Ok v -> Lwt.return (`Value (Wm.to_yojson v)))

let get_status (state : Protocol.state) ids _user _body _env _state =
  React.S.value state.notifs.status
  |> (fun l ->
    match ids with
    | [] -> l
    | ids -> List.filter (fun (x : Qoe_status.t) ->
                 List.exists (Application_types.Stream.ID.equal x.stream) ids) l)
  |> Util_json.List.to_yojson Qoe_status.to_yojson
  |> fun r -> Lwt.return (`Value r)


(*


let handlers (state : state) =
  [ State_handler.add_layer "pipeline"
      ; Pipeline_api_measurements.handler api
      ; Pipeline_api_history.handler api
      ]
  ]
 *)
