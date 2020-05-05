type t = < list_timezones : string list Lwt.t
       ; timezone : string Lwt.t
       ; set_timezone : string -> unit Lwt.t
       ; set_local_rtc : bool -> unit Lwt.t
       ; local_rtc : bool Lwt.t
       ; ntp : bool Lwt.t
       ; set_ntp : bool -> unit Lwt.t
       ; time : Time.t Lwt.t
       ; set_time : Time.t -> unit Lwt.t
       ; local_time : Time.t Lwt.t >

val make : unit -> t Lwt.t
