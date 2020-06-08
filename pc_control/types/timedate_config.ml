type t = {
  timezone : string;
  ntp : bool;
  local_time : Time.t;
  ntp_server : string option;
  ntp_ip : Netlib.Ipaddr.V4.t option;
}
[@@deriving yojson, eq]

let to_string t = t |> to_yojson |> Yojson.Safe.pretty_to_string
