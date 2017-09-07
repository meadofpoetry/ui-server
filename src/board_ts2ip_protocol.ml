
[@@@ocaml.warning "-32"]
[%%cstruct
 type header =
   { prefix   : uint16_t
   ; msg_code : uint16_t
   } [@@little_endian]]

[%%cstruct
 type board_info =
   { board_type    : uint8_t
   ; board_version : uint8_t
   ; packers_num   : uint8_t
   ; rfu           : uint8_t
   } [@@little_endian]]

[%%cstruct
 type factory_settings =
   { mac : uint16_t [@len 3]
   ; rfu : uint16_t [@len 61]
   } [@@little_endian]]

[%%cstruct
 type packer_settings =
   { self_port : uint16_t
   ; dst_mac   : uint16_t [@len 3]
   ; dst_ip    : uint32_t
   ; dst_port  : uint16_t
   ; stream_id : uint32_t
   ; mode      : uint8_t
   ; rfu       : uint8_t
   } [@@little_endian]]

[%%cstruct
 type req_settings_main =
   { cmd      : uint16_t
   ; ip       : uint32_t
   ; mask     : uint32_t
   ; gateway  : uint32_t
   ; rfu      : uint16_t [@len 17]
   } [@@little_endian]]

[%%cstruct
 type req_settings_packers =
   { cmd : uint16_t
   ; rfu : uint16 [@len 3]
   } [@@little_endian]]

[%%cstruct
 type status =
   { rfu_1     : uint16_t
   ; phy       : uint16_t
   ; rfu_2     : uint16_t
   ; byterates : uint32_t [@len 30]
   } [@@little_endian]]

[@@@ocaml.warning "+32"]

(* ------------------- Misc ------------------- *)

let prefix    = 0x55AA
let self_port = 2028

let to_header ~msg_code () =
  let hdr = Cstruct.create sizeof_header in
  let ()  = set_header_prefix hdr prefix in
  let ()  = set_header_msg_code hdr msg_code in
  hdr

let to_msg ~msg_code ~body () =
  let hdr = to_header ~msg_code () in
  Cstruct.append hdr body

type info =
  { typ         : int
  ; ver         : int
  ; packers_num : int option
  }

type factory_settings =
  { mac : Macaddr.t
  }

type packer_settings =
  { stream_id : int32 (* FIXME *)
  ; port_id   : int
  ; dst_ip    : Ipaddr.V4.t
  ; dst_port  : int
  ; enabled   : bool
  }

type settings =
  { ip      : Ipaddr.V4.t
  ; mask    : Ipaddr.V4.t
  ; gateway : Ipaddr.V4.t
  ; packers : packer_settings list
  }

type speed = | Speed10
             | Speed100
             | Speed1000
             | Speed_failure

type packer_status =
  { bitrate  : int32 option
  ; enabled  : bool
  ; overflow : bool
  }

type status =
  { phy_ok  : bool
  ; speed   : speed
  ; link_ok : bool
  ; packers : packer_status list
  }

(* ------------------- Requests/responses ------------------- *)

(* Get board info *)

let to_req_get_board_info = to_header ~msg_code:0x0080

let of_rsp_get_board_info msg =
  let packers_num = (match get_board_info_packers_num msg with
                     | 0 -> Some 8
                     | _ -> None
                    ) in
  { typ = get_board_info_board_type msg
  ; ver = get_board_info_board_version msg
  ; packers_num
  }

(* Set factory settings  *)

let to_req_set_factory_settings ~settings =
  let body = Cstruct.create sizeof_factory_settings in
  let ()   = set_factory_settings_mac (Macaddr.to_string settings.mac) 0 body in
  to_msg ~msg_code:0x0087 ~body

(* Set settings *)

(* Insert packer data into main message *)
let blit_packer i p offset body =
  let dstoff = i * sizeof_packer_settings + offset in
  let buf    = Cstruct.create sizeof_packer_settings in
  let mode   = p.port_id lsl 1 |> fun x -> if p.enabled then x lor 1 else x in
  let ()     = Ipaddr.V4.to_int32 p.dst_ip |> set_packer_settings_dst_ip buf in
  let ()     = p.dst_port                  |> set_packer_settings_dst_port buf in
  let ()     = Ipaddr.V4.multicast_to_mac p.dst_ip |> Macaddr.to_string
               |> fun mac -> set_packer_settings_dst_mac mac 0 buf in
  let ()     = set_packer_settings_self_port buf self_port in
  let ()     = set_packer_settings_mode buf mode in
  let ()     = set_packer_settings_stream_id buf p.stream_id in
  Cstruct.blit buf 0 body dstoff sizeof_packer_settings

let pack_main s p =
  let body = Cstruct.create (sizeof_req_settings_main + (4 * sizeof_packer_settings)) in
  let ()   = set_req_settings_main_cmd body 0 in
  let ()   = Ipaddr.V4.to_int32 s.ip      |> set_req_settings_main_ip body in
  let ()   = Ipaddr.V4.to_int32 s.mask    |> set_req_settings_main_mask body in
  let ()   = Ipaddr.V4.to_int32 s.gateway |> set_req_settings_main_gateway body in
  let ()   = List.iteri (fun i x -> blit_packer i x sizeof_req_settings_main body) p in
  to_msg ~msg_code:0x0088 ~body ()

let to_req_set_settings ~(settings:settings) =
  let ppm         = 6 in (* packers per message *)
  let max_packers = ppm * 3 in
  let req_num     = List.length settings.packers - 4
                    |> (fun x -> if x > max_packers then max_packers else if x < 0 then 0 else x)
                    |> (fun x -> ( x / ppm ) + (if ( x mod ppm ) > 0 then 1 else 0))
                    |> (fun x -> x + 1) in
  let rec pack = (fun acc req_id l ->
      (match req_id with
       | 0 -> acc
       | x when x = req_num -> let (hd,tl) = CCList.take_drop 4 l in
                               let msg = pack_main settings @@ CCList.take 4 l in
                               pack (msg::acc) (req_id - 1) tl
       | x -> let body    = Cstruct.create (sizeof_req_settings_packers + (ppm * sizeof_packer_settings)) in
              let (hd,tl) = CCList.take_drop ppm l in
              let ()      = set_req_settings_packers_cmd body req_id in
              let ()      = List.iteri (fun i x -> blit_packer i x sizeof_req_settings_packers body) hd in
              let msg     = to_msg ~msg_code:0x0088 ~body () in
              pack (msg::acc) (req_id - 1) tl)) in
  List.rev (pack [] req_num settings.packers)

(* ------------------- Events ------------------- *)

(* Status *)

let of_status msg =
  let phy = get_status_phy msg in
  { phy_ok  = if (phy land 1) > 0 then true else false
  ; speed   = if (phy land 0x06) > 0 then Speed1000
              else if (phy land 0x0A) > 0 then Speed100
              else if (phy land 0x12) > 0 then Speed10
              else Speed_failure
  ; link_ok = if (phy land 0x20) > 0 then true else false
  ; packers = []
  }
