open Netlib
open Application_types

(* Physical port on a board. *)
type socket =
  | SPI_1
  | SPI_2
  | SPI_3
  | ASI_1
  | ASI_2 [@@deriving yojson, show, eq, enum]

type speed =
  | Speed_10
  | Speed_100
  | Speed_1000
  | Speed_failure [@@deriving yojson, show, eq]

type devinfo =
  { typ : int
  ; ver : int
  ; packers_num : int
  } [@@deriving yojson, eq]

type udp_mode =
  { stream : Stream.Multi_TS_ID.t
  ; dst_ip : Ipaddr.V4.t
  ; dst_port : int
  ; self_port : int
  ; enabled : bool
  ; socket : socket
  } [@@deriving yojson, show, eq]

type network_mode =
  { ip : Ipaddr.V4.t
  ; mask : Ipaddr.V4.t
  ; gateway : Ipaddr.V4.t
  } [@@deriving yojson, show, eq]

type mode =
  { network : network_mode
  ; udp : udp_mode list
  } [@@deriving yojson, show, eq]

type device_status =
  { phy : bool
  ; link : bool
  ; speed : speed
  ; sync : socket list
  ; timestamp : Time.t
  } [@@deriving yojson, show, eq]

type udp_status =
  { bitrate : int option
  ; overflow : bool
  ; enabled : bool
  ; sync : bool
  ; stream : Stream.Multi_TS_ID.t
  } [@@deriving yojson, show, eq]

type transmitter_status =
  { udp : udp_status list
  ; timestamp : Time.t
  } [@@deriving yojson, show, eq]

type config =
  { mac : Macaddr.t
  ; mode : mode
  } [@@deriving yojson, eq]

type 'a ts =
  { data : 'a
  ; timestamp : Time.t
  } [@@deriving yojson, show, eq]

let socket_to_string = function
  | ASI_1 -> "ASI 1"
  | ASI_2 -> "ASI 2"
  | SPI_1 -> "SPI 1"
  | SPI_2 -> "SPI 2"
  | SPI_3 -> "SPI 3"

let devinfo_to_string (x : devinfo) =
  Printf.sprintf "type: 0x%02X, version: %d, packers: %d"
    x.typ x.ver x.packers_num
