open Netlib

type t = Board_niitv_ts2ip_types.config

let equal = Board_niitv_ts2ip_types.equal_config

let to_yojson = Board_niitv_ts2ip_types.config_to_yojson

let of_yojson = Board_niitv_ts2ip_types.config_of_yojson

let default =
  { Board_niitv_ts2ip_types.mac = Macaddr.of_string_exn "00:50:c2:88:50:ab"
  ; mode =
      { network =
          { ip = Ipaddr.V4.make 192 168 0 1
          ; mask = Ipaddr.V4.make 255 255 255 0
          ; gateway = Ipaddr.V4.make 192 168 0 1 }
      ; udp = [] } }

let to_string x = Yojson.Safe.to_string @@ to_yojson x

let of_string x =
  Yojson.Safe.from_string x
  |> of_yojson
  |> function
  | Ok v -> v
  | Error e -> failwith e
