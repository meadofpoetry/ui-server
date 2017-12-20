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

type info =
  { typ         : int
  ; ver         : int
  ; packers_num : int option
  } [@@deriving yojson]

type factory_settings =
  { mac : Macaddr.t
  } [@@deriving yojson]

type settings =
  { ip      : Ipaddr.V4.t
  ; mask    : Ipaddr.V4.t
  ; gateway : Ipaddr.V4.t
  ; packers : packer_settings list
  }
and packer_settings =
  { stream_id : unit (* FIXME *)
  ; port_id   : int
  ; dst_ip    : Ipaddr.V4.t
  ; dst_port  : int
  ; enabled   : bool
  } [@@deriving yojson]

type speed = Speed10
           | Speed100
           | Speed1000
           | Speed_failure [@@deriving yojson]

type status =
  { phy_ok  : bool
  ; speed   : speed
  ; link_ok : bool
  ; packers : packer_status list
  }
and packer_status =
  { bitrate  : int option
  ; enabled  : bool
  ; overflow : bool
  } [@@deriving yojson]

type config = { board_mode   : settings
              ; factory_mode : factory_settings
              } [@@deriving yojson]
let config_to_string c = Yojson.Safe.to_string @@ config_to_yojson c
let config_of_string s = config_of_yojson @@ Yojson.Safe.from_string s

let config_default =
  { board_mode = { ip = Ipaddr.V4.make 192 168 111 200
                 ; mask = Ipaddr.V4.make 255 255 255 0
                 ; gateway = Ipaddr.V4.make 192 168 111 1
                 ; packers = [] }
  ; factory_mode = { mac = Macaddr.of_string_exn "00:50:c2:88:ac:ab"}
  }
