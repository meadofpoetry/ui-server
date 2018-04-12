open Containers

module Macaddr = struct
  include Macaddr

  let to_yojson x = `String (to_string x)
  let of_yojson = function
    | `String s -> (match of_string s with
                    | Some x -> Ok x
                    | None   -> Error ("Bad macaddress string: " ^ s))
    | _ -> Error "Macaddress json should be a string"
end

module Ipaddr = struct
  module V4 = struct
    include Ipaddr.V4

    let to_yojson x = `String (to_string x)
    let of_yojson = function
      | `String s -> (match of_string s with
                      | Some a -> Ok a
                      | None   -> Error ("Bad ipaddr: " ^ s))
      | _ -> Error "Ipaddr json should be a string"
  end
end

type devinfo =
  { typ         : int
  ; ver         : int
  ; packers_num : int option
  } [@@deriving yojson]

type factory_settings =
  { mac : Macaddr.t
  } [@@deriving yojson]

type stream_setting =
  { stream   : Common.Stream.t
  ; dst_ip   : Ipaddr.V4.t
  ; dst_port : int
  ; enabled  : bool
  } [@@deriving yojson]

type packer_setting =
  { base      : stream_setting
  ; stream_id : int32 (* stream id to listen to *)
  ; port      : int   (* physical port on a board where this stream should be found *)
  ; self_port : int
  } [@@deriving yojson]

type nw_settings     =
  { ip      : Ipaddr.V4.t
  ; mask    : Ipaddr.V4.t
  ; gateway : Ipaddr.V4.t
  } [@@deriving yojson]

type speed = Speed10
           | Speed100
           | Speed1000
           | Speed_failure [@@deriving yojson]

type board_status =
  { phy_ok  : bool
  ; speed   : speed
  ; link_ok : bool
  } [@@deriving yojson]
type packer_status =
  { bitrate  : int option
  ; enabled  : bool
  ; has_data : bool
  ; overflow : bool
  } [@@deriving yojson]
type status_data = General of packer_status list
                 | Unknown of string

type config = { nw_mode      : nw_settings
              ; factory_mode : factory_settings
              ; streams      : packer_setting list
              } [@@deriving yojson]
let config_to_string c = Yojson.Safe.to_string @@ config_to_yojson c
let config_of_string s = config_of_yojson @@ Yojson.Safe.from_string s

let config_default =
  { nw_mode      = { ip      = Ipaddr.V4.make 192 168 111 200
                   ; mask    = Ipaddr.V4.make 255 255 255 0
                   ; gateway = Ipaddr.V4.make 192 168 111 1
                   }
  ; factory_mode = { mac = Macaddr.of_string_exn "00:50:c2:88:50:ab" }
  ; streams      = []
  }


type devinfo_response       = devinfo option [@@deriving yojson]
type config_response        = { nw_mode      : nw_settings
                              ; factory_mode : factory_settings
                              ; streams      : stream_setting list
                              } [@@deriving yojson]
type streams_full_request   = stream_setting list [@@deriving yojson]
type status                 = { board_status   : board_status
                              ; packers_status : (stream_setting * packer_status) list
                              } [@@deriving yojson]

let config_to_config_response (c:config) : config_response =
  { nw_mode      = c.nw_mode
  ; factory_mode = c.factory_mode
  ; streams      = List.map (fun x -> x.base) c.streams
  }
