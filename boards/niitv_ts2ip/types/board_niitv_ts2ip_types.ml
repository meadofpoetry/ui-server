open Netlib
open Application_types

let ( % ) f g x = f (g x)

(* Physical port on a board. *)
type socket =
  | SPI_1
  | SPI_2
  | SPI_3
  | ASI_1
  | ASI_2 [@@deriving show, eq, enum]

let socket_to_string = function
  | ASI_1 -> "ASI 1"
  | ASI_2 -> "ASI 2"
  | SPI_1 -> "SPI 1"
  | SPI_2 -> "SPI 2"
  | SPI_3 -> "SPI 3"

let socket_to_yojson = Util_json.Int.to_yojson % socket_to_enum

let socket_of_yojson json =
  match Util_json.Int.of_yojson json with
  | Error _ as e -> e
  | Ok x -> match socket_of_enum x with
    | Some x -> Ok x
    | None -> Error (Printf.sprintf "socket_of_yojson: bad int value (%d)" x)

let stream_to_socket
    (ports : Topology.topo_port list)
    (stream : Stream.t) : socket option =
  match Stream.to_topo_port ports stream with
  | None -> None
  | Some p -> socket_of_enum p.port

type speed =
  | Speed_10
  | Speed_100
  | Speed_1000
  | Speed_failure [@@deriving enum, show, eq]

let speed_to_yojson = Util_json.Int.to_yojson % speed_to_enum

let speed_of_yojson json =
  match Util_json.Int.of_yojson json with
  | Error _ as e -> e
  | Ok x -> match speed_of_enum x with
    | Some x -> Ok x
    | None -> Error (Printf.sprintf "speed_of_yojson: bad int value (%d)" x)

type devinfo =
  { typ : int
  ; ver : int
  ; packers_num : int
  } [@@deriving yojson, eq]

type udp_mode =
  { stream : Stream.Multi_TS_ID.t
  ; stream_id : Stream.ID.t option [@default None]
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
  ; stream : Stream.container_id
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

let devinfo_to_string (x : devinfo) =
  Printf.sprintf "type: 0x%02X, version: %d, packers: %d"
    x.typ x.ver x.packers_num
