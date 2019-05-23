open Board_dektec_dtm3200_types
open Application_types
open Api_util

module Event = struct
  open Util_react

  let get_state (api : Protocol.api) _user _body _env _state =
    let event =
      S.changes api.notifs.state
      |> E.map Topology.state_to_yojson in
    Lwt.return (`Ev event)

  let get_info (api : Protocol.api) _user _body _env _state =
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
  return_value
  @@ Topology.state_to_yojson
  @@ React.S.value api.notifs.state

let get_info (api : Protocol.api) _user _body _env _state =
  match React.S.value api.notifs.devinfo with
  | None -> return_error Request.Not_responding
  | Some x -> return_value @@ devinfo_to_yojson x

let get_config (api : Protocol.api) _user _body _env _state =
  return_value @@ config_to_yojson @@ React.S.value api.notifs.config

let get_fpga_version (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Device FPGA_version)
  >>=? return_value % Util_json.Int.to_yojson

let get_hardware_version (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Device Hardware_version)
  >>=? return_value % Util_json.Int.to_yojson

let get_firmware_version (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Device Firmware_version)
  >>=? return_value % Util_json.Int.to_yojson

let get_serial_number (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Device Serial_number)
  >>=? return_value % Util_json.Int.to_yojson

let get_type (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Device Type)
  >>=? return_value % Util_json.Int.to_yojson
