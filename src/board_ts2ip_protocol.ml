
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
  let hdr = Cbuffer.create sizeof_header in
  let ()  = set_header_prefix hdr prefix in
  let ()  = set_header_msg_code hdr msg_code in
  hdr

let to_msg ~msg_code ~body () =
  let hdr = to_header ~msg_code () in
  Cbuffer.append hdr body

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
  let body = Cbuffer.create sizeof_factory_settings in
  let ()   = set_factory_settings_mac (Macaddr.to_string settings.mac) 0 body in
  to_msg ~msg_code:0x0087 ~body

(* Set settings *)

(* Insert packer data into main message *)
let blit_packer i p offset body =
  let dstoff = i * sizeof_packer_settings + offset in
  let buf    = Cbuffer.create sizeof_packer_settings in
  let mode   = p.port_id lsl 1 |> fun x -> if p.enabled then x lor 1 else x in
  let ()     = Ipaddr.V4.to_int32 p.dst_ip |> set_packer_settings_dst_ip buf in
  let ()     = p.dst_port                  |> set_packer_settings_dst_port buf in
  let ()     = Ipaddr.V4.multicast_to_mac p.dst_ip |> Macaddr.to_string
               |> fun mac -> set_packer_settings_dst_mac mac 0 buf in
  let ()     = set_packer_settings_self_port buf self_port in
  let ()     = set_packer_settings_mode buf mode in
  let ()     = set_packer_settings_stream_id buf p.stream_id in
  Cbuffer.blit buf 0 body dstoff sizeof_packer_settings

let pack_main s p =
  let body = Cbuffer.create (sizeof_req_settings_main + (4 * sizeof_packer_settings)) in
  let ()   = set_req_settings_main_cmd body 0 in
  let ()   = Ipaddr.V4.to_int32 s.ip      |> set_req_settings_main_ip body in
  let ()   = Ipaddr.V4.to_int32 s.mask    |> set_req_settings_main_mask body in
  let ()   = Ipaddr.V4.to_int32 s.gateway |> set_req_settings_main_gateway body in
  let ()   = List.iteri (fun i x -> blit_packer i x sizeof_req_settings_main body) p in
  to_msg ~msg_code:0x0088 ~body ()

let to_req_set_settings ~(settings:settings) () =
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
       | x -> let body    = Cbuffer.create (sizeof_req_settings_packers + (ppm * sizeof_packer_settings)) in
              let (hd,tl) = CCList.take_drop ppm l in
              let ()      = set_req_settings_packers_cmd body req_id in
              let ()      = List.iteri (fun i x -> blit_packer i x sizeof_req_settings_packers body) hd in
              let msg     = to_msg ~msg_code:0x0088 ~body () in
              pack (msg::acc) (req_id - 1) tl)) in
  List.rev (pack [] req_num settings.packers)

(* ------------------- Events ------------------- *)

(* Status *)

let of_status msg =
  let packers = Cbuffer.split_size 4 @@ get_status_byterates msg
                |> List.filter (fun x -> Cbuffer.len x > 0)
                |> List.map (fun x -> let ( land ) = Int32.logand in
                                      let ( * )    = Int32.mul in
                                      let b        = Cbuffer.LE.get_uint32 x 0 in
                                      { bitrate  = (if (b land 0x10000000l) > 0l
                                                    then Some ((b land 0x0FFFFFFFl) * 8l)
                                                    else None)
                                      ; enabled  = (b land 0x40000000l) > 0l
                                      ; overflow = (b land 0x80000000l) > 0l
                            }) in
  let phy = get_status_phy msg in
  { phy_ok  = if (phy land 1) > 0 then true else false
  ; speed   = if (phy land 0x06) > 0 then Speed1000
              else if (phy land 0x0A) > 0 then Speed100
              else if (phy land 0x12) > 0 then Speed10
              else Speed_failure
  ; link_ok = if (phy land 0x20) > 0 then true else false
  ; packers
  }

(* ------------------- Board protocol implementation ------------------- *)

type instant = Board_meta.instant

type event = Status of status
           | Board_info of info

type response = Board_info of info

type _ request = Get_board_info   : response request
               | Set_board_mode   : settings         -> instant request
               | Set_factory_mode : factory_settings -> instant request

let (detect : response request) = Get_board_info

let (init : response request list) = [(* factory mode
                                       board mode*)]

let probes = [(* status, heartbeat *)]

let period = 5

let serialize : type a. a request -> Cbuffer.t = function
  | Set_board_mode settings   -> Cbuffer.concat @@ to_req_set_settings ~settings ()
  | Get_board_info            -> to_req_get_board_info ()
  | Set_factory_mode settings -> to_req_set_factory_settings ~settings ()

(* Deserialization *)

type err = Bad_prefix of int
         | Bad_length of int
         | Bad_msg_code of int
         | No_prefix_after_msg
         | Insufficient_payload of Cbuffer.t
         | Unknown_err of string

let string_of_err = function
  | Bad_prefix x            -> "incorrect prefix: " ^ (string_of_int x)
  | Bad_length x            -> "incorrect length: " ^ (string_of_int x)
  | Bad_msg_code x          -> "incorrect code: "   ^ (string_of_int x)
  | No_prefix_after_msg     -> "no prefix found after message payload"
  | Insufficient_payload _  -> "insufficient payload"
  | Unknown_err s           -> s

let check_prefix buf =
  let prefix' = get_header_prefix buf in
  if prefix != prefix then Error (Bad_prefix prefix') else Ok buf

let check_rest_and_crop buf =
  let hdr,res = Cbuffer.split buf sizeof_header in
  let code    = get_header_msg_code buf in
  try
    let length = (match code with
                  | 0x0100 -> sizeof_board_info
                  | 0x0F40 -> sizeof_status
                  | _      -> failwith "unknown message code") in
    if length > (512 - sizeof_header)
    then Error (Bad_length length)
    else let body,next_data = Cbuffer.split res length in
         let valid_msg      = Cbuffer.append hdr body in
         if Cbuffer.len next_data < sizeof_header
         then Ok valid_msg
         else (match check_prefix next_data with
               | Ok _    -> Ok valid_msg
               | Error _ -> Error No_prefix_after_msg)
  with
  | Invalid_argument _ -> Error (Insufficient_payload buf)
  | Failure _          -> Error (Bad_msg_code code)
  | e                  -> Error (Unknown_err (Printexc.to_string e))

let get_msg buf =
  try
    CCResult.(check_prefix buf
              >>= check_rest_and_crop)
  with e -> Error (Unknown_err (Printexc.to_string e))

let deserialize buf =
  let parse_msg = fun msg ->
    try
      let code   = get_header_msg_code msg in
      let _,body = Cbuffer.split msg sizeof_header in
      (match code with
       | 0x0100 -> `E (Board_info (of_rsp_get_board_info body) : event)
       | 0x0F40 -> `E (Status (of_status body) : event)
       | _      -> `N)
    with e -> `N in
  let rec f events responses b =
    if Cbuffer.len b < sizeof_header
    then List.rev events, List.rev responses, b
    else (match get_msg b with
          | Ok msg  -> let _,res = Cbuffer.split b (Cbuffer.len msg) in
                       parse_msg msg |> (function
                                         | `E x -> f (x::events) responses res
                                         | `R x -> f events (x::responses) res
                                         | `N   -> f events responses res)
          | Error e -> (match e with
                        | Insufficient_payload x -> List.rev events, List.rev responses, x
                        | _                      -> Cbuffer.split b 1 |> fun (_,x) -> f events responses x)) in
  let events,responses,res = f [] [] buf in
  events,responses, if Cbuffer.len res > 0 then Some res else None

let is_response (type a) (req: a request) (resp:a) =
  match (req,resp) with
  | Get_board_info, Board_info _ -> Some resp
  | _                            -> None

let pp _ = ()
