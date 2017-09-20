open Common__board_types

(* Overall types *)
type mode        = Asi2ip | Ip2asi
type application = Failsafe | Normal
type storage     = Flash | Ram

(* Ip types *)
type status    = Enabled | Disabled | Failure [@@deriving yojson]
type protocol  = Udp | Rtp [@@deriving yojson]
type output    = Asi | Spi 
type packet_sz = Ts188 | Ts204 [@@deriving yojson]
type rate_mode = On | Fixed | Without_pcr | Off [@@deriving yojson]

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

let ipv4_to_yojson (a : Ipaddr.V4.t) : Yojson.Safe.json =
  let s = Ipaddr.V4.to_string a in
  `String s

let ipv4_of_yojson = function
  | `String s -> (match Ipaddr.V4.of_string s with
                  | Some a -> Ok a
                  | None -> Error ("bad address: " ^ s))
  | _ -> Error "not an ip addr"
             
type addr = Ipaddr.V4.t
let addr_to_yojson : addr -> Yojson.Safe.json = ipv4_to_yojson
let addr_of_yojson : Yojson.Safe.json -> (addr, string) result = ipv4_of_yojson

type mask = Ipaddr.V4.t
let mask_to_yojson : mask -> Yojson.Safe.json = ipv4_to_yojson
let mask_of_yojson : Yojson.Safe.json -> (mask, string) result = ipv4_of_yojson          

type gateway = Ipaddr.V4.t
let gateway_to_yojson : gateway -> Yojson.Safe.json = ipv4_to_yojson
let gateway_of_yojson : Yojson.Safe.json -> (gateway, string) result = ipv4_of_yojson             

type port = int [@@deriving yojson]

type multicast = Ipaddr.V4.t
let multicast_to_yojson : multicast -> Yojson.Safe.json = ipv4_to_yojson
let multicast_of_yojson : Yojson.Safe.json -> (multicast, string) result = ipv4_of_yojson

type delay = int [@@deriving yojson]

type flag = bool [@@deriving yojson]

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
  } [@@deriving yojson]

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
