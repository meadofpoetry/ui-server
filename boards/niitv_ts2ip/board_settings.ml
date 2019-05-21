open Netlib
open Application_types

type t = Board_niitv_ts2ip_types.config

let equal = Board_niitv_ts2ip_types.equal_config

let to_yojson = Board_niitv_ts2ip_types.config_to_yojson

let of_yojson = Board_niitv_ts2ip_types.config_of_yojson

let default =
  let info =
    { Stream.Source.
      freq = 586_000_000L
    ; plp = 0
    ; bw = 8.0
    } in
  let node = { Topology. input = ASI; id = 1 } in
  let source =
    { Stream.
      info = DVB_T2 info
    ; node = Entry (Input node)
    } in
  let stream =
    { Stream.
      source
    ; typ = TS
    ; id = Stream.ID.of_string "460b38ee-186b-5604-8811-235eb3005960"
    ; orig_id = TS_multi (Stream.Multi_TS_ID.of_int32_pure 5l)
    } in
  { Board_niitv_ts2ip_types.
    mac = Macaddr.of_string_exn "00:50:c2:88:50:ab"
  ; mode =
      { network =
          { ip = Ipaddr.V4.make 192 168 0 1
          ; mask = Ipaddr.V4.make 255 255 255 0
          ; gateway = Ipaddr.V4.make 192 168 0 1
          }
      ; udp =
          [ { stream = Full stream
            ; dst_ip = Ipaddr.V4.make 224 1 2 2
            ; dst_port = 1234
            ; self_port = 2027
            ; enabled = true
            ; socket = SPI_2
            }
          ]
      }
  }

let to_string x =
  Yojson.Safe.to_string @@ to_yojson x

let of_string x =
  Yojson.Safe.from_string x
  |> of_yojson
  |> function Ok v -> v | Error e -> failwith e
