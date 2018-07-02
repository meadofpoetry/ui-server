open Common

(* Overall types *)
type mode        = Asi2ip | Ip2asi
type application = Failsafe | Normal
type storage     = Flash | Ram

let mode_to_string = function
  | Asi2ip -> "ASI to IP"
  | Ip2asi -> "IP to ASI"

let application_to_string = function
  | Failsafe -> "failsafe"
  | Normal   -> "normal"

let storage_to_string = function
  | Flash -> "flash"
  | Ram   -> "RAM"

(* Ip types *)
type receiver_status = Enabled | Disabled | Failure [@@deriving yojson, show]
type protocol        = Udp | Rtp [@@deriving yojson, show]
type meth            = Unicast | Multicast [@@deriving yojson, eq, show]
type output          = Asi | Spi
type packet_sz       = Ts188 | Ts204 [@@deriving yojson, show]
type rate_mode       = On | Fixed | Without_pcr | Off [@@deriving yojson,eq]

let meth_to_string = function
  | Unicast   -> "unicast"
  | Multicast -> "multicast"

let rate_mode_to_string = function
  | On          -> "on"
  | Fixed       -> "fixed"
  | Without_pcr -> "without PCR"
  | Off         -> "off"

(* Asi types *)
type asi_packet_sz = Sz of packet_sz | As_is

type devinfo =
  { fpga_ver : int
  ; hw_ver   : int
  ; fw_ver   : int
  ; serial   : int
  ; typ      : int
  ; mac      : Macaddr.t
  } [@@deriving yojson]

type status =
  { fec_delay       : int
  ; fec_cols        : int
  ; fec_rows        : int
  ; jitter_tol      : int
  ; lost_after_fec  : int64
  ; lost_before_fec : int64
  ; tp_per_ip       : int
  ; status          : receiver_status
  ; protocol        : protocol
  ; packet_size     : packet_sz
  ; bitrate         : int
  ; pcr_present     : bool
  ; rate_change_cnt : int32
  ; jitter_err_cnt  : int32
  ; lock_err_cnt    : int32
  ; delay_factor    : int32
  ; asi_bitrate     : int
  } [@@deriving yojson]

let protocol_to_string = function
  | Udp -> "UDP"
  | Rtp -> "RTP"

let packet_sz_to_string = function
  | Ts188 -> "188"
  | Ts204 -> "204"

type nw = { ip      : Ipaddr.V4.t
          ; mask    : Ipaddr.V4.t
          ; gateway : Ipaddr.V4.t
          ; dhcp    : bool
          } [@@deriving yojson,eq]

type ip = { enable    : bool
          ; fec       : bool
          ; port      : int
          ; multicast : Ipaddr.V4.t option
          ; delay     : int
          ; rate_mode : rate_mode
          } [@@deriving yojson,eq]

type config = { nw : nw
              ; ip : ip
              } [@@deriving yojson,eq]

let config_to_string c = Yojson.Safe.to_string @@ config_to_yojson c

let config_of_string s = config_of_yojson @@ Yojson.Safe.from_string s

let config_default = { nw = { ip        = Ipaddr.V4.of_string_exn "192.168.111.68"
                            ; mask      = Ipaddr.V4.of_string_exn "255.255.255.0"
                            ; gateway   = Ipaddr.V4.of_string_exn "192.168.111.1"
                            ; dhcp      = false
                            }
                     ; ip = { enable    = true
                            ; fec       = true
                            ; port      = 1234
                            ; multicast = Some (Ipaddr.V4.of_string_exn "224.1.2.2")
                            ; delay     = 100
                            ; rate_mode = On
                            }
                     }
