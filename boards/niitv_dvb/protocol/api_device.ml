open Board_niitv_dvb_types.Device
open Application_types
open Api_util

module Event = struct
  open Util_react

  let get_state (api : Protocol.api) _user _body _env _state =
    let event =
      S.changes api.notifs.state
      |> E.map Topology.state_to_yojson in
    Lwt.return (`Ev event)

  let get_receivers (api : Protocol.api) _user _body _env _state =
    let event =
      S.map ~eq:(Util_equal.(Option.equal @@ List.equal (=))) (function
          | None -> None
          | Some x -> Some x.receivers) api.notifs.devinfo
      |> S.changes
      |> E.map Util_json.(Option.to_yojson @@ List.to_yojson Int.to_yojson) in
    Lwt.return (`Ev event)

  let get_mode (api : Protocol.api) (ids : int list) _user _body _env _state =
    let to_yojson = Util_json.(
        List.to_yojson (Pair.to_yojson Int.to_yojson mode_to_yojson)) in
    let event = match ids with
      | [] ->
        S.changes api.notifs.config
        |> E.map (fun ({ mode; _ } : config) -> to_yojson mode)
      | ids ->
        S.changes api.notifs.config
        |> E.fmap (fun ({ mode; _ } : config) ->
            match List.filter (fun (id, _) -> List.mem id ids) mode with
            | [] -> None
            | l -> Some (to_yojson l)) in
    Lwt.return (`Ev event)
end

let reset (api : Protocol.api) _user _body _env _state =
  api.channel Reset
  >>=? return_value % info_to_yojson

let get_state (api : Protocol.api) _user _body _env _state =
  return_value
  @@ Topology.state_to_yojson
  @@ React.S.value api.notifs.state

let get_info (api : Protocol.api) force _user _body _env _state =
  match force with
  | Some true ->
    api.channel Get_devinfo
    >>=? return_value % info_to_yojson
  | None | Some false ->
    match React.S.value api.notifs.devinfo with
    | None -> return_error Request.Not_responding
    | Some x -> return_value @@ info_to_yojson x

let get_receivers (api : Protocol.api) _user _body _env _state =
  match React.S.value api.notifs.devinfo with
  | None -> return_error Request.Not_responding
  | Some x ->
    let to_yojson = Util_json.(List.to_yojson Int.to_yojson) in
    return_value @@ to_yojson x.receivers

let get_mode (api : Protocol.api) (ids : int list) _user _body _env _state =
  api.kv#get
  >>= fun (config : config) ->
  let value = match ids with
    | [] -> config.mode
    | ids -> List.filter (fun (id, _) -> List.mem id ids) @@ config.mode in
  let to_yojson = Util_json.(
      List.to_yojson (Pair.to_yojson Int.to_yojson mode_to_yojson)) in
  return_value @@ to_yojson value
