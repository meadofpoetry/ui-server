open Pc_control_types

let ( let* ) = Lwt.bind

let get_config (state : Timedate.t) _user _body _env _state =
  let* timezone = state.time_config#timezone in
  let* ntp = state.time_config#ntp in
  let* local_time = state.time_config#local_time in
  let* (ntp_server, ntp_ip) =
    if not ntp then Lwt.return (None, None)
    else
      let* ntp_server = state.ntp_config#ntp_server in
      let* ntp_ip = state.ntp_config#ntp_server_ip in
      Lwt.return (Some ntp_server, Some ntp_ip)
  in
  let js = Timedate_config.to_yojson { timezone; ntp; local_time; ntp_server; ntp_ip } in
  Lwt.return (`Value js)

let get_timezones (state : Timedate.t) _user _body _env _state =
  let js = Util_json.List.to_yojson (fun s -> `String s) state.timezones in
  Lwt.return (`Value js)

let set_timezone (state : Timedate.t) zone _user _body _env _state =
  if (List.exists (fun z -> z = zone) state.timezones)
  then
    let* () = state.time_config#set_timezone zone in
    Lwt.return `Unit
  else
    Lwt.return (`Error ("zone " ^ zone ^ " is unknown"))

let set_ntp (state : Timedate.t) flag _user _body _env _state =
  let* () = state.time_config#set_ntp flag in
  Lwt.return `Unit

let set_time (state : Timedate.t) time _user _body _env _state =
  let* () = state.time_config#set_time time in
  Lwt.return `Unit
