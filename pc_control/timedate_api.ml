open Pc_control_types

let ( let* ) = Lwt.bind

module Event = struct
  open Util_react

  let get_config (state : Timedate.t) _user =
    let event = E.map Timedate_config.to_yojson state.updates in
    Lwt.return event
end

let get_config (state : Timedate.t) _user _body _env _state =
  let* conf = Timedate.read_config state in
  let js = Timedate_config.to_yojson conf in
  Lwt.return (`Value js)

let get_timezones (state : Timedate.t) _user _body _env _state =
  let js = Util_json.List.to_yojson (fun s -> `String s) state.timezones in
  Lwt.return (`Value js)

let set_timezone (state : Timedate.t) zone _user _body _env _state =
  if List.exists (fun z -> z = zone) state.timezones then
    let* () = state.time_config#set_timezone zone in
    let* () = Timedate.push_update state in
    Lwt.return `Unit
  else Lwt.return (`Error ("zone " ^ zone ^ " is unknown"))

let set_ntp (state : Timedate.t) flag _user _body _env _state =
  let* () = state.time_config#set_ntp flag in
  let* () = Timedate.push_update state in
  Lwt.return `Unit

let set_time (state : Timedate.t) time _user _body _env _state =
  let* () = state.time_config#set_time time in
  let* () = Timedate.push_update state in
  Lwt.return `Unit
