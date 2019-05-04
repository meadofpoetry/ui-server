open Board_niitv_ts2ip_types

type error =
  | Timeout
  | Queue_overflow
  | Not_responding
  | Invalid_length
  | Invalid_payload

let error_to_string = function
  | Timeout -> "timeout"
  | Queue_overflow -> "queue overflow"
  | Not_responding -> "not responding"
  | Invalid_length -> "invalid length"
  | Invalid_payload -> "invalid payload"

type tag =
  [ `Devinfo_req
  | `Devinfo_rsp
  | `MAC
  | `Mode
  | `Status
  ] [@@deriving eq, show]

type msg =
  { tag : tag
  ; data : Cstruct.t
  }

type _ t =
  (* Requests device info. *)
  | Get_devinfo : devinfo t
  (* Sets network *)
  | Set_mode_main : mode -> unit t
  (* Sets UDP mode for packers from 11 to 22. *)
  | Set_mode_aux_1 : udp_mode list -> unit t
  (* Sets UDP mode for packers from 23 to 34. *)
  | Set_mode_aux_2 : udp_mode list -> unit t
  (* Sets MAC address. *)
  | Set_mac : Netlib.Macaddr.t -> unit t

let timeout (type a) : a t -> float = function
  | Get_devinfo -> 3.
  | Set_mode_main _ -> 0.
  | Set_mode_aux_1 _ -> 0.
  | Set_mode_aux_2 _ -> 0.
  | Set_mac _ -> 0.

let value_to_string (type a) (t : a t) (v : a) : string option =
  match t with
  | Get_devinfo -> Some (devinfo_to_string v)
  | Set_mode_main _ -> None
  | Set_mode_aux_1 _ -> None
  | Set_mode_aux_2 _ -> None
  | Set_mac _ -> None

let to_string (type a) : a t -> string = function
  | Get_devinfo -> "Get devinfo"
  | Set_mode_main _ -> "Set mode (main)"
  | Set_mode_aux_1 _ -> "Set mode (aux 1)"
  | Set_mode_aux_2 _ -> "Set mode (aux 2)"
  | Set_mac _ -> "Set MAC address"

let req_tag_to_enum : tag -> int = function
  | `Devinfo_req -> 0x0080
  | `Devinfo_rsp -> 0x0140
  | `MAC -> 0x0087
  | `Mode -> 0x0088
  | `Status -> 0x0F40

let rsp_tag_of_enum : int -> tag option = function
  | 0x0087 -> Some `MAC
  | 0x0088 -> Some `Mode
  | 0x0F40 -> Some `Status
  | 0x0080 -> Some `Devinfo_req
  | 0x0140 -> Some `Devinfo_rsp
  | _ -> None

let to_tag (type a) : a t -> tag = function
  | Get_devinfo -> `Devinfo_req
  | Set_mode_main _ -> `Mode
  | Set_mode_aux_1 _ -> `Mode
  | Set_mode_aux_2 _ -> `Mode
  | Set_mac _ -> `MAC

let tag_to_data_size : tag -> int = function
  | `Status -> Message.sizeof_status
  | `Devinfo_rsp -> Message.sizeof_rsp_devinfo
  | `Mode -> Message.sizeof_req_mode_main
  | `MAC -> Message.sizeof_req_factory_mode
  | `Devinfo_req -> 0

let take_drop (n : int) (l : 'a list) =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | l when i = 0 -> List.rev acc, l
    | hd :: tl -> aux (pred i) (hd :: acc) tl
  in
  aux n [] l

let split_mode (mode : mode) =
  let main_pkrs, aux = take_drop Message.n_udp_main mode.udp in
  let aux_1, aux_2 = take_drop Message.n_udp_aux aux in
  { mode with udp = main_pkrs }, aux_1, aux_2
