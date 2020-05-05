type t = < ntp_server : string Lwt.t
       ; ntp_server_ip : Netlib.Ipaddr.V4.t Lwt.t >

val make : unit -> t Lwt.t
