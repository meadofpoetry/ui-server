let ( let* ) = Lwt.bind

module Timedate_options = Kv_v.RW (Pc_control_types.Timedate_config)

type t =
  { timezones : string list
  ; time_config : Timedate1.t
  ; ntp_config : Timesync1.t
  ; options : Timedate_options.t
  }

let read_config state =
  let open Pc_control_types.Timedate_config in
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
  Lwt.return { timezone; ntp; local_time; ntp_server; ntp_ip }

let update_options state =
  let* value = read_config state in
  state.options#set value

let create (kv : Kv.RW.t) =
  let ( let* ) = Lwt.bind in
  let ( let*? ) = Lwt_result.bind in
  let options_path = [ "pc"; "timedate" ] in
  let default = Pc_control_types.Timedate_config.default in
  let*? options = Timedate_options.create ~default kv options_path in
  let* time_config = Timedate1.make () in
  let* ntp_config = Timesync1.make () in
  let* timezones = time_config#list_timezones in
  let* () = time_config#set_local_rtc true in
  let state = { timezones; time_config; ntp_config; options } in
  let* () = update_options state in
  Lwt.return_ok state
