open Pc_control_types

let ( let* ) = Lwt.bind

type t = {
  timezones : string list;
  time_config : Timedate1.t;
  updates : Timedate_config.t React.event;
  push_updates : Timedate_config.t -> unit;
}

let read_ntp_config () =
  Lwt.catch
    (fun () ->
      let* ntp_config = Timesync1.make () in
      let* ntp_server = ntp_config#ntp_server in
      let* ntp_ip = ntp_config#ntp_server_ip in
      Lwt.return (Some ntp_server, Some ntp_ip))
    (fun _ ->
      Lwt.return (None, None))

let read_config state =
  let open Pc_control_types.Timedate_config in
  let* timezone = state.time_config#timezone in
  let* ntp = state.time_config#ntp in
  let* local_time = state.time_config#time in
  let* ntp_server, ntp_ip =
    if not ntp then Lwt.return (None, None)
    else read_ntp_config ()
  in
  Lwt.return { timezone; ntp; local_time; ntp_server; ntp_ip }

let push_update state =
  let* conf = read_config state in
  state.push_updates conf;
  Lwt.return_unit

let create () =
  let ( let* ) = Lwt.bind in
  let* time_config = Timedate1.make () in
  let* timezones = time_config#list_timezones in
  (* Local clock is UTC I guess *)
  let* () = time_config#set_local_rtc false in
  let updates, push_updates = React.E.create () in
  Lwt.return { timezones; time_config; updates; push_updates }
