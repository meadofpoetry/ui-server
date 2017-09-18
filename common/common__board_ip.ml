
(* Overall types *)
type mode        = Asi2ip | Ip2asi
type application = Failsafe | Normal
type storage     = Flash | Ram

(* Ip types *)
type status    = Enabled | Disabled | Failure [@@deriving to_yojson]
type protocol  = Udp | Rtp [@@deriving to_yojson]
type output    = Asi | Spi 
type packet_sz = Ts188 | Ts204 [@@deriving to_yojson]
type rate_mode = On | Fixed | Without_pcr | Off

(* Asi types *)
type asi_packet_sz = Sz of packet_sz | As_is

module Macaddr = struct
  include Macaddr
  let to_yojson x = `String (Macaddr.to_string x)
end

type devinfo = { fpga_ver : int
               ; hw_ver   : int
               ; fw_ver   : int
               ; serial   : int
               ; typ      : int
               ; mac      : Macaddr.t
               } [@@deriving to_yojson]

type nw_settings =
  { ip      : Ipaddr.V4.t
  ; mask    : Ipaddr.V4.t
  ; gateway : Ipaddr.V4.t
  ; dhcp    : bool
  }

type ip_settings =
  { enable    : bool
  ; fec       : bool
  ; port      : int
  ; multicast : Ipaddr.V4.t option
  ; delay     : int option
  ; rate_mode : rate_mode option
  }

type board_status =
  { fec_delay       : int
  ; fec_cols        : int
  ; fec_rows        : int
  ; jitter_tol      : int
  ; lost_after_fec  : int64
  ; lost_before_fec : int64
  ; tp_per_ip       : int
  ; status          : status
  ; protocol        : protocol
  ; packet_size     : packet_sz
  ; bitrate         : int
  ; pcr_present     : bool
  ; rate_change_cnt : int32
  ; jitter_err_cnt  : int32
  ; lock_err_cnt    : int32
  ; delay_factor    : int32
  ; asi_bitrate     : int
  } [@@deriving to_yojson]

let status_to_string = function
  | Enabled -> "Channel is enabled; No errors detected"
  | Disabled -> "Channel has been disabled"
  | Failure -> "Channel is enabled, but there is a problem when processing of the received IP stream"

let protocol_to_string = function
  | Udp -> "UDP"
  | Rtp -> "RTP"

let packet_sz_to_string = function
  | Ts188 -> "Ts 188"
  | Ts204 -> "Ts 204"
