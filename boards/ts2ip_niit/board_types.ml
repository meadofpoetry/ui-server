open Containers
open Common

type devinfo =
  { typ         : int
  ; ver         : int
  ; packers_num : int option
  } [@@deriving yojson]

type factory_settings =
  { mac : Macaddr.t
  } [@@deriving yojson]

type nw_settings     =
  { ip      : Ipaddr.V4.t
  ; mask    : Ipaddr.V4.t
  ; gateway : Ipaddr.V4.t
  } [@@deriving yojson]

type stream_settings =
  { stream   : Stream.t
  ; dst_ip   : Ipaddr.V4.t
  ; dst_port : int
  ; enabled  : bool
  } [@@deriving yojson]

type packer_settings =
  { stream    : Stream.id (* stream id to listen to *)
  ; dst_ip    : Ipaddr.V4.t
  ; dst_port  : int
  ; self_port : int
  ; enabled   : bool
  ; socket    : int       (* physical port on a board where this stream should be found *)
  } [@@deriving yojson]

type packers_error =
  [ `Limit_exceeded of (int * int)
  | `Undefined_limit
  ] [@@deriving yojson]

let packers_error_to_string = function
  | `Undefined_limit ->
     "Undefined limit of streams. Probably the board is not responding"
  | `Limit_exceeded (exp,got) ->
     Printf.sprintf "Limit exceeded. Got %d, but only %d is available" got exp

type speed = Speed_10
           | Speed_100
           | Speed_1000
           | Speed_failure [@@deriving yojson]

type board_status =
  { phy_ok  : bool
  ; link_ok : bool
  ; speed   : speed
  } [@@deriving yojson]

type packer_status =
  { bitrate  : int option
  ; enabled  : bool
  ; has_data : bool
  ; overflow : bool
  } [@@deriving yojson]

type status_data =
  | General of packer_status list
  | Unknown of string

type status  =
  { board_status   : board_status
  ; packers_status : (packer_settings * packer_status) list
  } [@@deriving yojson]

type config =
  { nw_mode      : nw_settings
  ; factory_mode : factory_settings
  ; packers      : packer_settings list
  } [@@deriving yojson]

let config_to_string c = Yojson.Safe.to_string @@ config_to_yojson c
let config_of_string s = config_of_yojson @@ Yojson.Safe.from_string s

let config_default =
  { nw_mode      = { ip      = Ipaddr.V4.make 192 168 111 200
                   ; mask    = Ipaddr.V4.make 255 255 255 0
                   ; gateway = Ipaddr.V4.make 192 168 111 1
                   }
  ; factory_mode = { mac = Macaddr.of_string_exn "00:50:c2:88:50:ab" }
  ; packers      = []
  }
