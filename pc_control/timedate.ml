type t =
  { timezones : string list
  ; time_config : Timedate1.t
  ; ntp_config : Timesync1.t
  }

let create () =
  let ( let* ) = Lwt.bind in
  let* time_config = Timedate1.make () in
  let* ntp_config = Timesync1.make () in
  let* timezones = time_config#list_timezones in
  let* () = time_config#set_local_rtc true in
  Lwt.return { timezones; time_config; ntp_config }
