type t =
  { timezone : string
  ; ntp : bool
  ; local_time : Time.t
  ; ntp_server : string option
  ; ntp_ip : Netlib.Ipaddr.V4.t option
  } [@@deriving yojson, eq]
