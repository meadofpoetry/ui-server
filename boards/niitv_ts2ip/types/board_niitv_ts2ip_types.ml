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
  { stream : Stream.t (* stream id to listen to *)
  ; dst_ip : Ipaddr.V4.t
  ; dst_port : int
  ; self_port : int
  ; enabled : bool
  ; socket : socket
  } [@@deriving yojson, show, eq]

type udp_mode_simple =
  { stream : Stream.t
  ; dst_ip : Ipaddr.V4.t
  ; dst_port : int
  ; enabled : bool
  } [@@deriving yojson, show, eq]

type mode =
  { ip : Ipaddr.V4.t
  ; mask : Ipaddr.V4.t
  ; gateway : Ipaddr.V4.t
  ; udp : udp_mode list
  } [@@deriving yojson, show, eq]

type packers_error =
  [ `Limit_exceeded of (int * int)
  | `Undefined_limit
  ] [@@deriving yojson]

let packers_error_to_string = function
  | `Undefined_limit ->
    "Undefined limit of streams. Probably the board is not responding"
  | `Limit_exceeded (exp,got) ->
    Printf.sprintf "Limit exceeded. Got %d, but only %d is available" got exp

type udp_status =
  { bitrate : int option
  ; overflow : bool
  ; enabled : bool
  ; sync : bool
  ; stream : Stream.Multi_TS_ID.t
  } [@@deriving yojson, show, eq]

type status =
  { phy : bool
  ; link : bool
  ; speed : speed
  ; asi_1 : bool
  ; asi_2 : bool
  ; spi_1 : bool
  ; spi_2 : bool
  ; spi_3 : bool
  ; udp : udp_status list
  } [@@deriving yojson, show, eq]

type config =
  { mac : Macaddr.t
  ; mode : mode
  } [@@deriving yojson, eq]
