open Pc_control_types

let ( let* ) = Lwt.bind

let time_lock = Lwt_mutex.create ()

module Event = struct
  open Util_react
  
  let get_config (state : Timedate.t) _user =
    let event = React.E.map Timedate_config.to_yojson state.updates in
    Lwt.return event

  (*
  let get_config (state : Timedate.t) _user =
    let on_update _ =
      let* conf = Timedate.read_config state in
      let js = Timedate_config.to_yojson conf in
      Lwt.return js
    in
    let* signal = state.time_config#signal in
    let event =
      S.changes signal
      |> E.map_p on_update
    in
    Lwt.return event
   *)
end

let get_config (state : Timedate.t) _user _body _env _state =
  Lwt_mutex.with_lock time_lock (fun () ->
      let* conf = Timedate.read_config state in
      let* () =
        Logs_lwt.info (fun m ->
            m "Timedate requested config: %s" (Timedate_config.to_string conf))
      in
      let js = Timedate_config.to_yojson conf in
      Lwt.return (`Value js))

let get_timezones (state : Timedate.t) _user _body _env _state =
  Lwt_mutex.with_lock time_lock (fun () ->
      let js = Util_json.List.to_yojson (fun s -> `String s) state.timezones in
      let* () = Logs_lwt.info (fun m -> m "Timedate requested timezones") in
      Lwt.return (`Value js))

let set_timezone (state : Timedate.t) zone _user _body _env _state =
  Lwt_mutex.with_lock time_lock (fun () ->
      if List.exists (fun z -> z = zone) state.timezones then
        let* () = state.time_config#set_timezone zone in
        let* () =
          Logs_lwt.info (fun m ->
              m "Timedate requested timezone update: %s" zone)
        in
        (* God forgive me *)
        let timer = Lwt_timeout.create 1 (fun () ->
                        Lwt.async (fun () -> Timedate.update_event state))
        in
        Lwt_timeout.start timer;
        Lwt.return `Unit
      else Lwt.return (`Error ("zone " ^ zone ^ " is unknown")))

let set_ntp (state : Timedate.t) flag _user _body _env _state =
  Lwt_mutex.with_lock time_lock (fun () ->
      let* () = state.time_config#set_ntp flag in
      let* () =
        Logs_lwt.info (fun m ->
            m "Timedate requested ntp state update: %b" flag)
      in
      (* This is not how it should be done *)
      let timer = Lwt_timeout.create 1 (fun () ->
                      Lwt.async (fun () -> Timedate.update_event state))
      in
      Lwt_timeout.start timer;
      Lwt.return `Unit)

let set_time (state : Timedate.t) time _user _body _env _state =
  Lwt_mutex.with_lock time_lock (fun () ->
      let* () = state.time_config#set_time time in
      let* () =
        Logs_lwt.info (fun m ->
            let tz_offset_s = Ptime_clock.current_tz_offset_s () in
            m "Timedate requested time update: %s"
              (Time.to_human_string ?tz_offset_s time))
      in
      (* This is not how it should be done *)
      let timer = Lwt_timeout.create 2 (fun () ->
                      Lwt.async (fun () -> Timedate.update_event state))
      in
      Lwt_timeout.start timer;
      Lwt.return `Unit)
