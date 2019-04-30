type t = Board_niitv_dvb_types.Device.config

let equal = Board_niitv_dvb_types.Device.equal_config

let to_yojson = Board_niitv_dvb_types.Device.config_to_yojson

let of_yojson = Board_niitv_dvb_types.Device.config_of_yojson

let default : t = [
  0, Board_niitv_dvb_types.{ standard = T2; channel = { freq = 666_000_000; bw = Bw8; plp = 0 }}
]

let to_string x =
  Yojson.Safe.to_string @@ to_yojson x

let of_string x =
  Yojson.Safe.from_string x
  |> of_yojson
  |> function Ok v -> v | Error e -> failwith e
