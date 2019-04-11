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

let mode_to_int = function
  | ASI2IP -> 0
  | IP2ASI -> 1

let mode_of_int = function
  | 0 -> Some ASI2IP
  | 1 -> Some IP2ASI
  | _ -> None

let application_to_int = function
  | Failsafe -> 0
  | Normal -> 1

let application_of_int = function
  | 0 -> Some Failsafe
  | 1 -> Some Normal
  | _ -> None

let storage_to_int = function
  | FLASH -> 0
  | RAM -> 1

let storage_of_int = function
  | 0 -> Some FLASH
  | 1 -> Some RAM
  | _ -> None

let meth_to_int = function
  | Unicast -> 0
  | Multicast -> 1

let meth_of_int = function
  | 0 -> Some Unicast
  | 1 -> Some Multicast
  | _ -> None

let rate_mode_to_int = function
  | On -> 0
  | Off -> 3
  | Fixed -> 1
  | Without_pcr -> 2

let rate_mode_of_int = function
  | 0 -> Some On
  | 1 -> Some Fixed
  | 2 -> Some Without_pcr
  | 3 -> Some Off
  | _ -> None

let receiver_status_of_int : int -> receiver_status option = function
  | 0 -> Some On
  | 1 -> Some Off
  | 2 -> Some Fail
  | _ -> None

let protocol_of_int = function
  | 0 -> Some UDP
  | 1 -> Some RTP
  | _ -> None

let output_of_int = function
  | 0 -> Some ASI
  | 1 -> Some SPI
  | _ -> None

let packet_sz_of_int = function
  | 0 -> Some TS188
  | 1 -> Some TS204
  | _ -> None

let asi_packet_sz_to_int = function
  | Sz TS188 -> 0
  | Sz TS204 -> 1
  | As_is -> 2

let asi_packet_sz_of_int = function
  | 0 -> Some (Sz TS188)
  | 1 -> Some (Sz TS204)
  | 2 -> Some (As_is)
  | _ -> None
