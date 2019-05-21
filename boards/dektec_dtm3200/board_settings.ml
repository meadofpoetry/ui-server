open Netlib

type t = Board_dektec_dtm3200_types.config

let equal = Board_dektec_dtm3200_types.equal_config

let to_yojson = Board_dektec_dtm3200_types.config_to_yojson

let of_yojson = Board_dektec_dtm3200_types.config_of_yojson

let (default : t) =
  { nw =
      { ip_address = Ipaddr.V4.of_string_exn "192.168.0.2"
      ; mask = Ipaddr.V4.of_string_exn "255.255.255.0"
      ; gateway = Ipaddr.V4.of_string_exn "192.168.0.1"
      ; dhcp = false
      }
  ; ip_receive =
      { enable = false
      ; fec_enable = true
      ; udp_port = 1234
      ; addressing_method = Multicast
      ; multicast = Ipaddr.V4.of_string_exn "224.1.2.2"
      ; ip_to_output_delay = 100
      ; rate_mode = On
      }
  ; address = 0x40
  }

let to_string x =
  Yojson.Safe.to_string @@ to_yojson x

let of_string x =
  Yojson.Safe.from_string x
  |> of_yojson
  |> function Ok v -> v | Error e -> failwith e
