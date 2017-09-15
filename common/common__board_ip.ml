
(* Overall types *)
type mode        = Asi2ip | Ip2asi
type application = Failsafe | Normal
type storage     = Flash | Ram

(* Ip types *)
type status    = Enabled | Disabled | Failure
type protocol  = Udp | Rtp
type output    = Asi | Spi
type packet_sz = Ts188 | Ts204
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
