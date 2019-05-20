open Board_dektec_dtm3200_types
open Application_types

let ( >>= ) = Lwt.( >>= )

module Event = struct
  open Util_react

  let get_state (api : Protocol.api) _user _body _env _state =
    let event =
      S.changes api.notifs.state
      |> E.map Topology.state_to_yojson in
    Lwt.return (`Ev event)

  let get_devinfo (api : Protocol.api) _user _body _env _state =
    let event =
      S.changes api.notifs.devinfo
      |> E.map (Util_json.Option.to_yojson devinfo_to_yojson) in
    Lwt.return (`Ev event)

  let get_config (api : Protocol.api) _user _body _env _state =
    let event =
      S.changes api.notifs.config
      |> E.map config_to_yojson in
    Lwt.return (`Ev event)
end

let get_state (api : Protocol.api) _user _body _env _state =
  let value = Topology.state_to_yojson @@ React.S.value api.notifs.state in
  Lwt.return (`Value value)

let get_devinfo (api : Protocol.api) _user _body _env _state =
  let value = React.S.value api.notifs.devinfo in
  Lwt.return (`Value Util_json.(Option.to_yojson devinfo_to_yojson value))

let get_config (api : Protocol.api) _user _body _env _state =
  let value = React.S.value api.notifs.config in
  Lwt.return (`Value (config_to_yojson value))

let reboot (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Network Reboot)
  >>= function
  | Ok () -> Lwt.return `Unit
  | Error e -> Lwt.return @@ `Error (Request.error_to_string e)
