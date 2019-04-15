open Netlib

(* Overall types *)
type mode =
  | ASI2IP
  | IP2ASI [@@deriving eq, enum]

type application =
  | Failsafe
  | Normal [@@deriving eq, enum]

type storage =
  | FLASH
  | RAM [@@deriving eq, enum]

(* Ip types *)
type receiver_status =
  | On
  | Off
  | Fail [@@deriving yojson, eq, enum]

type protocol =
  | UDP
  | RTP [@@deriving yojson, eq, enum]

type meth =
  | Unicast
  | Multicast [@@deriving yojson, eq, enum]

type output =
  | ASI
  | SPI [@@deriving enum]

type packet_sz =
  | TS188
  | TS204 [@@deriving yojson, eq, enum]

type rate_mode =
  | On
  | Fixed
  | Without_pcr
  | Off [@@deriving yojson, eq, enum]

(* ASI types *)
type asi_packet_sz = Sz of packet_sz | Copy

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

let packet_sz_to_string = function
  | TS188 -> "188"
  | TS204 -> "204"

let meth_to_string = function
  | Unicast -> "Unicast"
  | Multicast -> "Multicast"

let mode_to_string = function
  | IP2ASI -> "IP to ASI"
  | ASI2IP -> "ASI to IP"

let application_to_string = function
  | Failsafe -> "failsafe"
  | Normal -> "normal"

let storage_to_string = function
  | FLASH -> "Flash"
  | RAM -> "RAM"

let rate_mode_to_string = function
  | On -> "on"
  | Off -> "off"
  | Fixed -> "fixed"
  | Without_pcr -> "without PCR"

let receiver_status_to_string : receiver_status -> string = function
  | On -> "on"
  | Off -> "off"
  | Fail -> "fail"

let protocol_to_string = function
  | UDP -> "UDP"
  | RTP -> "RTP"

let asi_packet_sz_to_string = function
  | Copy -> "copy"
  | Sz x -> packet_sz_to_string x

let asi_packet_sz_to_int = function
  | Sz TS188 -> 0
  | Sz TS204 -> 1
  | Copy -> 2

let asi_packet_sz_of_int = function
  | 0 -> Some (Sz TS188)
  | 1 -> Some (Sz TS204)
  | 2 -> Some Copy
  | _ -> None
