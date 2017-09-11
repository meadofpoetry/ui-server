[@@@ocaml.warning "-32"]

[%%cenum
 type e_overall_mode =
   | ASI2IP
   | IP2ASI [@@uint8_t]]

[%%cenum
 type e_overall_app =
   | Failsafe
   | Normal [@@uint8_t]]

[%%cenum
 type e_overall_storage =
   | ROM
   | RAM [@@uint8_t]]

[%%cenum
 type e_ip_method =
   | Unicast
   | Multicast [@@uint8_t]]

[%%cenum
 type e_ip_status =
   | Normal
   | Disabled
   | Errors [@@uint8_t]]

[%%cenum
 type e_ip_protocol =
   | Udp
   | Rtp [@@uint8_t]]

[%%cenum
 type e_ip_packet_size =
   | Ts188
   | Ts204 [@@uint8_t]]

[%%cenum
 type e_ip_rate_estimation =
   | With_pcr
   | Fixed_delay
   | Without_pcr
   | Disabled [@@uint8_t]]

[%%cenum
 type e_asi_packet_size =
   | Ts188
   | Ts204
   | As_is [@@uint8_t]]

[%%cstruct
 type prefix =
   { stx      : uint8_t
   ; address  : uint8_t [@len 2]
   ; category : uint8_t [@len 2]
   ; setting  : uint8_t [@len 2]
   ; rw       : uint8_t
   ; index    : uint8_t [@len 4]
   } [@@little_endian]]

[%%cstruct
 type suffix =
   { crc : uint8_t
   ; etx : uint8_t
   } [@@little_endian]]

[%%cstruct
 type setting8 =
   { data : uint8_t [@len 2]
   } [@@little_endian]]

[%%cstruct
 type setting16 =
   { data : uint8_t [@len 4]
   } [@@little_endian]]

[%%cstruct
 type setting32 =
   { data : uint8_t [@len 8]
   } [@@little_endian]]

[%%cstruct
 type setting48 =
   { data : uint8_t [@len 12]
   } [@@little_endian]]

[%%cstruct
 type setting64 =
   { data : uint8_t [@len 16]
   } [@@little_endian]]

[@@@ocaml.warning "+32"]

(* ------------------- Misc ------------------- *)

type setting_size = I8 of int
                  | I16 of int
                  | I32 of int32
                  | I48 of int64
                  | I64 of int64

type category = Devinfo | Overall | Nw | Ip | Asi

type rw = Read | Write | Fail

type parsed =
  { category : category
  ; setting  : int
  ; rw       : rw
  ; data     : int64
  }

let stx = 0x02
let etx = 0x03
let address = 0x40

let category_to_int = function
  | Devinfo -> 0x01 | Overall -> 0x02 | Nw -> 0x03 | Ip -> 0x81 | Asi -> 0x84
let category_of_int = function
  | 0x01 -> Some Devinfo | 0x02 -> Some Overall | 0x03 -> Some Nw
  | 0x81 -> Some Ip      | 0x84 -> Some Asi     | _    -> None
let category_to_max_setting_id = function
  | Devinfo -> 0x05 | Overall -> 0x03 | Nw -> 0x06 | Ip -> 0x19 | Asi -> 0x03

let rw_to_int = function
  | Read  -> int_of_char 'R'
  | Write -> int_of_char 'W'
  | Fail  -> int_of_char 'E'
let rw_of_int = function
  | x when (x = int_of_char 'R') || (x = int_of_char 'r') -> Some Read
  | x when (x = int_of_char 'W') || (x = int_of_char 'w') -> Some Write
  | x when (x = int_of_char 'E') || (x = int_of_char 'e') -> Some Fail
  | _ -> None

let get_setting_size = function
  | I8  _ -> sizeof_setting8  | I16 _ -> sizeof_setting16 | I32 _ -> sizeof_setting32
  | I48 _ -> sizeof_setting48 | I64 _ -> sizeof_setting64

let int_to_hex_string x =
  let size = get_setting_size x in
  (match x with
   | I8  x | I16 x -> Printf.sprintf "%X" x
   | I32 x         -> Printf.sprintf "%lX" x
   | I48 x | I64 x -> Printf.sprintf "%LX" x)
  |> fun s -> if (String.length s) < size
              then ((String.make (size -  String.length s) '0') ^ s)
              else s

let pack_setting x =
  let size = get_setting_size x in
  let f = match x with
    | I8  _ -> set_setting8_data  | I16 _ -> set_setting16_data | I32 _ -> set_setting32_data
    | I48 _ -> set_setting48_data | I64 _ -> set_setting64_data in
  let hs = int_to_hex_string x in
  Cbuffer.create size |> fun b -> f hs 0 b; b

let hex_string_of_ascii_buf x = "0x" ^ (Cstruct.to_string x)
let int_of_ascii_buf x        = hex_string_of_ascii_buf x |> int_of_string
let int32_of_ascii_buf x      = hex_string_of_ascii_buf x |> Int32.of_string
let int64_of_ascii_buf x      = hex_string_of_ascii_buf x |> Int64.of_string

let calc_crc msg =
  let _,body = Cbuffer.split msg 1 in
  let iter = Cbuffer.iter (fun _ -> Some 1)
                          (fun buf -> Cbuffer.get_uint8 buf 0)
                          body in
  Cbuffer.fold (fun acc el -> el + acc) iter 0
  |> fun x -> ((lnot x) + 1) land 0xFF

let to_prefix ~category ~setting ~rw () =
  let pfx = Cbuffer.create sizeof_prefix in
  let ()  = set_prefix_stx pfx stx in
  let ()  = set_prefix_address  (int_to_hex_string (I8 address)) 0 pfx in
  let ()  = set_prefix_category (int_to_hex_string (I8 (category_to_int category))) 0 pfx in
  let ()  = set_prefix_setting  (int_to_hex_string (I8 setting)) 0 pfx in
  let ()  = set_prefix_index    (int_to_hex_string (I16 0)) 0 pfx in
  let ()  = set_prefix_rw pfx @@ rw_to_int rw in
  pfx

let to_suffix ~crc =
  let sfx = Cbuffer.create sizeof_suffix in
  let ()  = set_suffix_crc sfx crc in
  let ()  = set_suffix_etx sfx etx in
  sfx

let to_msg ~category ~setting ~rw ~body () =
  let pfx = to_prefix ~category ~setting ~rw () in
  let msg = Cbuffer.append pfx body in
  let sfx = to_suffix ~crc:(calc_crc msg) in
  Cbuffer.append msg sfx

let to_empty_msg ~category ~setting ~rw =
  to_msg ~category ~setting ~rw ~body:(Cbuffer.create 0)

(* ------------------- Board protocol implementation ------------------- *)

type instant = Board_meta.instant

type resp = Devinfo_fpga_ver   of int
          | Devinfo_hw_ver     of int
          | Devinfo_fw_ver     of float
          | Devinfo_serial     of int
          | Devinfo_type       of int
          | Overall_mode       of e_overall_mode
          | Overall_app        of e_overall_app
          | Overall_storage    of e_overall_storage
          | Nw_ip              of Ipaddr.V4.t
          | Nw_mask            of Ipaddr.V4.t
          | Nw_gateway         of Ipaddr.V4.t
          | Nw_dhcp            of bool
          | Nw_reboot
          | Nw_mac             of Macaddr.t
          | Ip_method          of e_ip_method
          | Ip_enable          of bool
          | Ip_fec_delay       of int32
          | Ip_fec_enable      of bool
          | Ip_fec_cols        of int
          | Ip_fec_rows        of int
          | Ip_jitter_tol      of int32
          | Ip_lost_after_fec  of int64
          | Ip_lost_before_fec of int64
          | Ip_udp_port        of int
          | Ip_delay           of int
          | Ip_mcast_addr      of Ipaddr.V4.t
          | Ip_tp_per_ip       of int
          | Ip_status          of e_ip_status
          | Ip_protocol        of e_ip_protocol
          | Ip_packet_size     of e_ip_packet_size
          | Ip_bitrate         of int32
          | Ip_pcr_present     of bool
          | Ip_rate_change_cnt of int32
          | Ip_rate_estimation of e_ip_rate_estimation
          | Ip_jitter_err_cnt  of int32
          | Ip_lock_err_cnt    of int32
          | Ip_delay_factor    of int32
          | Asi_packet_size    of e_asi_packet_size
          | Asi_bitrate        of int32

type response = rw * resp

type _ request = Get_devinfo_fpga_ver   : response request
               | Get_devinfo_hw_ver     : response request
               | Get_devinfo_fw_ver     : response request
               | Get_devinfo_serial     : response request
               | Get_devinfo_type       : response request
               | Set_overall_mode       : e_overall_mode -> response request
               | Set_overall_app        : e_overall_app  -> response request
               | Set_overall_storage    : e_overall_storage -> response request
               | Set_nw_ip              : Ipaddr.V4.t -> response request
               | Set_nw_mask            : Ipaddr.V4.t -> response request
               | Set_nw_gateway         : Ipaddr.V4.t -> response request
               | Set_nw_dhcp            : bool -> response request
               | Reboot                 : response request
               | Get_nw_mac             : response request
               | Set_ip_method          : e_ip_method -> response request
               | Set_ip_enable          : bool -> response request
               | Get_ip_fec_delay       : response request
               | Set_ip_fec_enable      : bool -> response request
               | Get_ip_fec_columns     : response request
               | Get_ip_fec_rows        : response request
               | Get_ip_jitter_tol      : response request
               | Get_ip_lost_after_fec  : response request
               | Get_ip_lost_before_fec : response request
               | Set_ip_udp_port        : int -> response request
               | Set_ip_delay           : int -> response request
               | Set_ip_mcast_addr      : Ipaddr.V4.t -> response request
               | Get_ip_tp_per_ip       : response request
               | Get_ip_status          : response request
               | Get_ip_protocol        : response request
               | Get_ip_packet_size     : response request
               | Get_ip_bitrate         : response request
               | Get_ip_pcr_present     : response request
               | Get_ip_rate_change_cnt : response request
               | Get_ip_rate_estimation : response request
               | Get_ip_jitter_err_cnt  : response request
               | Get_ip_lock_err_cnt    : response request
               | Get_ip_delay_factor    : response request
               | Set_asi_packet_size    : e_asi_packet_size -> response request
               | Get_asi_bitrate        : response request

let (detect : response request) = Get_devinfo_serial

let (init : response request list) = []

let probes = []

let period = 5

type err = Bad_stx              of int
         | Bad_length           of int
         | Bad_address          of int
         | Bad_category         of int
         | Bad_setting          of int * int
         | Bad_rw               of int
         | Bad_crc              of int * int
         | Insufficient_payload of Cbuffer.t
         | No_stx_after_msg
         | Unknown_err          of string

let string_of_err = function
  | Bad_stx x              -> "incorrect STX: "          ^ (string_of_int x)
  | Bad_length x           -> "incorrect length: "       ^ (string_of_int x)
  | Bad_address x          -> "incorrect address: "      ^ (string_of_int x)
  | Bad_category x         -> "incorrect category: "     ^ (string_of_int x)
  | Bad_setting (x,y)      -> "incorrect setting: "      ^ (string_of_int x) ^ " in category = " ^ (string_of_int y)
  | Bad_rw x               -> "incorrect rw: "           ^ (string_of_int x)
  | Bad_crc (x,y)          -> "incorrect crc, expected " ^ (string_of_int x) ^ ", got " ^ (string_of_int y)
  | No_stx_after_msg       -> "no STX found after message payload"
  | Insufficient_payload _ -> "insufficient payload"
  | Unknown_err s          -> s

let get_setting_len c s =
  match c with
  | Devinfo -> (match s with
                | 0x01 | 0x02 -> Some sizeof_setting8
                | 0x03 | 0x04 | 0x05 -> Some sizeof_setting32
                | _ -> None)
  | Overall -> (match s with
                | x when x > 0 && x < 0x04 -> Some sizeof_setting8
                | _ -> None)
  | Nw      -> (match s with
                | 0x04 | 0x05 -> Some sizeof_setting8
                | 0x06        -> Some sizeof_setting48
                | x when x > 0 && x < 0x04 -> Some sizeof_setting32
                | _ -> None)
  | Ip      -> (match s with
                | 0x01 | 0x02 | 0x04 | 0x0D | 0x0E | 0x0F | 0x11 | 0x12 | 0x14 | 0x16 -> Some sizeof_setting8
                | 0x05 | 0x06 | 0x0A | 0x0B -> Some sizeof_setting16
                | 0x08 | 0x09 -> Some sizeof_setting64
                | 0x03 | 0x07 | 0x0C | 0x10 | 0x13 | 0x15 | 0x17 | 0x18 | 0x19 -> Some sizeof_setting32
                | _ -> None)
  | Asi     -> (match s with
                | 0x03 -> Some sizeof_setting32
                | 0x01 | 0x02 -> Some sizeof_setting8
                | _ -> None)

let check_stx buf =
  let stx' = get_prefix_stx buf in
  if stx <> stx' then Error (Bad_stx stx') else Ok buf

let check_address buf =
  let address' = get_prefix_address buf |> int_of_ascii_buf in
  if address' <> address then Error (Bad_address address') else Ok buf

let check_rw buf =
  let rw' = get_prefix_rw buf in
  match rw_of_int rw' with
  | Some _ -> Ok buf
  | None   -> Error (Bad_rw rw')

let check_crc msg =
  let _,bdy = Cbuffer.split msg sizeof_prefix in
  let _,sfx = Cbuffer.split bdy (Cbuffer.len bdy - sizeof_suffix - 1) in
  let crc   = get_suffix_crc sfx in
  let crc'  = calc_crc msg in
  if crc' <> crc then Error (Bad_crc (crc', crc)) else Ok msg

let check_rest buf =
  let category' = get_prefix_category buf |> int_of_ascii_buf in
  let _,msg'   = Cbuffer.split buf sizeof_prefix in
  let body,_   = Cbuffer.split msg' (Cbuffer.len msg' - sizeof_suffix - 1) in
  match category_of_int category' with
  | Some x -> let setting' = get_prefix_setting buf |> int_of_ascii_buf in
              let length   = get_setting_len x setting' in
              (match length with
               | Some len -> if (Cbuffer.len body) = len
                             then Ok { category = x
                                     ; setting  = setting'
                                     ; rw       = CCOpt.get_exn @@ rw_of_int @@ get_prefix_rw buf
                                     ; data     = int64_of_ascii_buf body
                                     }
                             else Error (Bad_length len)
               | None     -> Error (Bad_setting (setting', category')))
  | None   -> Error (Bad_category category')

let get_msg buf =
  try
    CCResult.(check_stx buf
              >>= check_address
              >>= check_crc
              >>= check_rest)
  with e -> Error (Unknown_err (Printexc.to_string e))

let parse_msg msg =
  let d  = msg.data in
  let rw = msg.rw in
  (match (msg.category, msg.setting) with
   | Devinfo, 0x01 -> Some (Devinfo_fpga_ver (Int64.to_int d))
   | Devinfo, 0x02 -> Some (Devinfo_hw_ver (Int64.to_int d))
   | Devinfo, 0x03 -> Some (Devinfo_fw_ver (Int64.to_float d /. 10.))
   | Devinfo, 0x04 -> Some (Devinfo_serial (Int64.to_int d))
   | Devinfo, 0x05 -> Some (Devinfo_type (Int64.to_int d))
   | _ -> None)
  |> (function
      | Some x -> Some (rw,x)
      | None   -> None)

let deserialize buf =
  let parts = Cbuffer.split_by_string (String.make 1 @@ char_of_int etx) buf in
  let rec f = fun acc x -> match x with
                           | []       -> acc, None
                           | [x]      -> (match get_msg x with
                                          | Ok msg  -> (match parse_msg msg with
                                                        | Some x -> x :: acc, None
                                                        | None   -> acc, None)
                                          | Error e -> (match e with
                                                        | Insufficient_payload res -> acc, Some res
                                                        | _                        -> acc, None))
                           | hd :: tl -> (match get_msg hd with
                                          | Ok msg  -> (match parse_msg msg with
                                                        | Some x -> f (x::acc) tl
                                                        | None   -> f acc tl)
                                          | Error _ -> f acc tl) in
  f [] parts

let to_req_get ~category ~setting =
  to_empty_msg ~category ~setting ~rw:Read

let to_req_set ~category ~setting x =
  let body = pack_setting x in
  to_msg ~category ~setting ~rw:Write ~body

let to_req_set_bool ~category ~setting b =
  let body = pack_setting (I8 (if b then 1 else 0)) in
  to_msg ~category ~setting ~rw:Write ~body

let to_req_set_ipaddr ~category ~setting ip =
  let body = pack_setting (I32 (Ipaddr.V4.to_int32 ip)) in
  to_msg ~category ~setting ~rw:Write ~body

let serialize : type a. a request -> Cbuffer.t = function
  | Get_devinfo_fpga_ver   -> to_req_get ~category:Devinfo ~setting:0x01 ()
  | Get_devinfo_hw_ver     -> to_req_get ~category:Devinfo ~setting:0x02 ()
  | Get_devinfo_fw_ver     -> to_req_get ~category:Devinfo ~setting:0x03 ()
  | Get_devinfo_serial     -> to_req_get ~category:Devinfo ~setting:0x04 ()
  | Get_devinfo_type       -> to_req_get ~category:Devinfo ~setting:0x05 ()
  | Set_overall_mode x     -> to_req_set ~category:Overall ~setting:0x01 (I8 (e_overall_mode_to_int x)) ()
  | Set_overall_app x      -> to_req_set ~category:Overall ~setting:0x02 (I8 (e_overall_app_to_int x)) ()
  | Set_overall_storage x  -> to_req_set ~category:Overall ~setting:0x03 (I8 (e_overall_storage_to_int x)) ()
  | Set_nw_ip ip           -> to_req_set_ipaddr ~category:Nw ~setting:0x01 ip ()
  | Set_nw_mask mask       -> to_req_set_ipaddr ~category:Nw ~setting:0x02 mask ()
  | Set_nw_gateway gw      -> to_req_set_ipaddr ~category:Nw ~setting:0x03 gw ()
  | Set_nw_dhcp dhcp       -> to_req_set_bool ~category:Nw ~setting:0x04 dhcp ()
  | Reboot                 -> to_req_set_bool ~category:Nw ~setting:0x05 true ()
  | Get_nw_mac             -> to_req_get ~category:Nw ~setting:0x06 ()
  | Set_ip_method x        -> to_req_set ~category:Ip ~setting:0x01 (I8 (e_ip_method_to_int x)) ()
  | Set_ip_enable x        -> to_req_set_bool ~category:Ip ~setting:0x02 x ()
  | Get_ip_fec_delay       -> to_req_get ~category:Ip ~setting:0x03 ()
  | Set_ip_fec_enable x    -> to_req_set_bool ~category:Ip ~setting:0x04 x ()
  | Get_ip_fec_columns     -> to_req_get ~category:Ip ~setting:0x05 ()
  | Get_ip_fec_rows        -> to_req_get ~category:Ip ~setting:0x06 ()
  | Get_ip_jitter_tol      -> to_req_get ~category:Ip ~setting:0x07 ()
  | Get_ip_lost_after_fec  -> to_req_get ~category:Ip ~setting:0x08 ()
  | Get_ip_lost_before_fec -> to_req_get ~category:Ip ~setting:0x09 ()
  | Set_ip_udp_port x      -> to_req_set ~category:Ip ~setting:0x0A (I16 x) ()
  | Set_ip_delay x         -> to_req_set ~category:Ip ~setting:0x0B (I16 x) ()
  | Set_ip_mcast_addr x    -> to_req_set_ipaddr ~category:Ip ~setting:0x0C x ()
  | Get_ip_tp_per_ip       -> to_req_get ~category:Ip ~setting:0x0D ()
  | Get_ip_status          -> to_req_get ~category:Ip ~setting:0x0E ()
  | Get_ip_protocol        -> to_req_get ~category:Ip ~setting:0x0F ()
  | Get_ip_packet_size     -> to_req_get ~category:Ip ~setting:0x12 ()
  | Get_ip_bitrate         -> to_req_get ~category:Ip ~setting:0x13 ()
  | Get_ip_pcr_present     -> to_req_get ~category:Ip ~setting:0x14 ()
  | Get_ip_rate_change_cnt -> to_req_get ~category:Ip ~setting:0x15 ()
  | Get_ip_rate_estimation -> to_req_get ~category:Ip ~setting:0x16 ()
  | Get_ip_jitter_err_cnt  -> to_req_get ~category:Ip ~setting:0x17 ()
  | Get_ip_lock_err_cnt    -> to_req_get ~category:Ip ~setting:0x18 ()
  | Get_ip_delay_factor    -> to_req_get ~category:Ip ~setting:0x19 ()
  | Set_asi_packet_size x  -> to_req_set ~category:Asi ~setting:0x01 (I8 (e_asi_packet_size_to_int x)) ()
  | Get_asi_bitrate        -> to_req_get ~category:Asi ~setting:0x03 ()
