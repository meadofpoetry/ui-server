open Netlib

(* Overall types *)
type mode =
  | ASI2IP
  | IP2ASI [@@deriving show]

type application =
  | Failsafe
  | Normal [@@deriving show]

type storage =
  | FLASH
  | RAM [@@deriving show]

(* Ip types *)
type receiver_status =
  | On
  | Off
  | Fail [@@deriving yojson, eq, show]

type protocol =
  | UDP
  | RTP [@@deriving yojson, eq, show]

type meth =
  | Unicast
  | Multicast [@@deriving yojson, eq, show]

type output =
  | ASI
  | SPI

type packet_sz =
  | TS188
  | TS204 [@@deriving yojson, eq, show]

type rate_mode =
  | On
  | Off
  | Fixed
  | Without_pcr [@@deriving yojson, eq, show]

(* ASI types *)
type asi_packet_sz = Sz of packet_sz | As_is

type devinfo =
  { fpga_ver : int
  ; hw_ver : int
  ; fw_ver : int
  ; serial : int
  ; typ : int
  ; mac : Macaddr.t
  } [@@deriving yojson, eq]

type status =
  { fec_delay : int
  ; fec_cols : int
  ; fec_rows : int
  ; jitter_tol : int
  ; lost_after_fec : int64
  ; lost_before_fec : int64
  ; tp_per_ip : int
  ; status : receiver_status
  ; protocol : protocol
  ; packet_size : packet_sz
  ; bitrate : int
  ; pcr_present : bool
  ; rate_change_cnt : int32
  ; jitter_err_cnt : int32
  ; lock_err_cnt : int32
  ; delay_factor : int32
  ; asi_bitrate : int
  } [@@deriving yojson, eq]

let protocol_to_string = function
  | UDP -> "UDP"
  | RTP -> "RTP"

let packet_sz_to_string = function
  | TS188 -> "188"
  | TS204 -> "204"

type nw =
  { ip : Ipaddr.V4.t
  ; mask : Ipaddr.V4.t
  ; gateway : Ipaddr.V4.t
  ; dhcp : bool
  } [@@deriving yojson, eq]

type ip =
  { enable : bool
  ; fec : bool
  ; port : int
  ; multicast : Ipaddr.V4.t option
  ; delay : int
  ; rate_mode : rate_mode
  } [@@deriving yojson, eq]

type config =
  { nw : nw
  ; ip : ip
  } [@@deriving yojson, eq]

let config_to_string c = Yojson.Safe.to_string @@ config_to_yojson c
let config_of_string s = config_of_yojson @@ Yojson.Safe.from_string s

let config_default =
  { nw =
      { ip = Ipaddr.V4.of_string_exn "192.168.0.1"
      ; mask = Ipaddr.V4.of_string_exn "255.255.255.0"
      ; gateway = Ipaddr.V4.of_string_exn "192.168.0.1"
      ; dhcp = false
      }
  ; ip =
      { enable = true
      ; fec = true
      ; port = 1234
      ; multicast = Some (Ipaddr.V4.of_string_exn "224.1.2.2")
      ; delay = 100
      ; rate_mode = On
      }
  }
