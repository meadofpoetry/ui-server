type t =
  { timezone : string
  ; ntp : bool
  ; local_time : Time.t
  ; ntp_server : string option
  ; ntp_ip : Netlib.Ipaddr.V4.t option
  } [@@deriving yojson, eq]

let to_string x = to_yojson x |> Yojson.Safe.to_string

let of_string x =
  Yojson.Safe.from_string x |> of_yojson |> function
  | Ok x -> x
  | Error e -> failwith e
