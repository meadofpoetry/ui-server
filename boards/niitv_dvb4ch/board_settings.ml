type t = Board_niitv_dvb4ch_types.Device.config

let equal = Board_niitv_dvb4ch_types.Device.equal_config

let to_yojson = Board_niitv_dvb4ch_types.Device.config_to_yojson

let of_yojson = Board_niitv_dvb4ch_types.Device.config_of_yojson

let default : t =
  { source = 1
  ; mode =
      [ 0, { Board_niitv_dvb4ch_types.Device.
             standard = T2
           ; channel = { freq = 586_000_000; bw = Bw8; plp = 0 }
           }
      ]
  }

let to_string x =
  Yojson.Safe.pretty_to_string @@ to_yojson x

let of_string x =
  Yojson.Safe.from_string x
  |> of_yojson
  |> function Ok v -> v | Error e -> failwith e
