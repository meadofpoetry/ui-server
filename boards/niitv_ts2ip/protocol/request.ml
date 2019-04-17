open Board_niitv_ts2ip_types

type req_tag =
  [ `Devinfo
  | `Factory_mode
  | `Mode
  ]

type rsp_tag =
  [ `Status
  | `Devinfo
  ]

type req =
  { tag : req_tag
  ; data : Cstruct.t
  }

type rsp =
  { tag : rsp_tag
  ; data : Cstruct.t
  }

type _ t =
  | Get_devinfo : devinfo t
  | Set_mode : nw_settings * packer_settings list -> unit t
  | Set_factory_mode : factory_settings -> unit t

let req_tag_to_enum : req_tag -> int = function
  | `Devinfo -> 0x0080
  | `Factory_mode -> 0x0087
  | `Mode -> 0x0088

let rsp_tag_to_enum : rsp_tag -> int = function
  | `Status -> 0x0F40
  | `Devinfo -> 0x0140

let to_req_tag (type a) : a t -> req_tag = function
  | Get_devinfo -> `Devinfo
  | Set_mode _ -> `Mode
  | Set_factory_mode _ -> `Factory_mode

let to_req (type a) (t : a t) : req =
  let tag = to_req_tag t in
  let data = match t with
    | Get_devinfo -> Cstruct.create 1
    | Set_factory_mode _ -> Cstruct.create 1
    | Set_mode _ -> Cstruct.create 1
  in
  { tag; data }
