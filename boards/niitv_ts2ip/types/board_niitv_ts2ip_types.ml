open Netlib
open Application_types

type devinfo =
  { typ : int
  ; ver : int
  ; packers_num : int option
  } [@@deriving yojson, eq]

type factory_settings =
  { mac : Macaddr.t
  } [@@deriving yojson, eq]

type nw_settings     =
  { ip : Ipaddr.V4.t
  ; mask : Ipaddr.V4.t
  ; gateway : Ipaddr.V4.t
  } [@@deriving yojson, show, eq]

type stream_settings =
  { stream : Stream.t
  ; dst_ip : Ipaddr.V4.t
  ; dst_port : int
  ; enabled : bool
  } [@@deriving yojson, show, eq]

type packer_settings =
  { stream : Stream.t (* stream id to listen to *)
  ; dst_ip : Ipaddr.V4.t
  ; dst_port : int
  ; self_port : int
  ; enabled : bool
  ; socket : int (* physical port on a board where this stream should be found *)
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

type speed =
  | Speed_10
  | Speed_100
  | Speed_1000
  | Speed_failure [@@deriving yojson, show, eq]

type board_status =
  { phy_ok : bool
  ; link_ok : bool
  ; speed : speed
  } [@@deriving yojson, show, eq]

type packer_status =
  { bitrate : int option
  ; enabled : bool
  ; has_data : bool
  ; overflow : bool
  } [@@deriving yojson, show, eq]

type status_data =
  | General of packer_status list
  | Unknown of string [@@deriving eq]

type status  =
  { board_status : board_status
  ; packers_status : (packer_settings * packer_status) list
  } [@@deriving yojson, show, eq]

type config =
  { nw_mode : nw_settings
  ; factory_mode : factory_settings
  ; packers : packer_settings list
  } [@@deriving yojson, eq]
