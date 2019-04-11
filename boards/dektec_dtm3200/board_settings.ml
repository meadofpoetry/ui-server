open Netlib

type t = Board_dektec_dtm3200_types.config

let equal = Board_dektec_dtm3200_types.equal_config

let to_yojson = Board_dektec_dtm3200_types.config_to_yojson

let of_yojson = Board_dektec_dtm3200_types.config_of_yojson

let (default : t) =
  { nw =
      { ip = Ipaddr.V4.of_string_exn "192.168.0.1"
      ; mask = Ipaddr.V4.of_string_exn "255.255.255.0"
      ; gateway = Ipaddr.V4.of_string_exn "192.168.0.1"
      ; dhcp = true
      }
  ; ip =
      { enable = false
      ; fec = true
      ; port = 1234
      ; multicast = Some (Ipaddr.V4.of_string_exn "224.1.2.2")
      ; delay = 100
      ; rate_mode = On
      }
  }

let to_string x =
  Yojson.Safe.to_string @@ to_yojson x

let of_string x =
  Yojson.Safe.from_string x
  |> of_yojson
  |> function Ok v -> v | Error e -> failwith e
