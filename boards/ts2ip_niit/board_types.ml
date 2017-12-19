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
