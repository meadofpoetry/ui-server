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
 type e_nw_method =
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
   { data : uint8_t [@len 1]
   } [@@little_endian]]

[%%cstruct
 type setting16 =
   { data : uint8_t [@len 2]
   } [@@little_endian]]

[%%cstruct
 type setting32 =
   { data : uint8_t [@len 4]
   } [@@little_endian]]

[%%cstruct
 type setting48 =
   { data : uint8_t [@len 6]
   } [@@little_endian]]

[%%cstruct
 type setting64 =
   { data : uint8_t [@len 8]
   } [@@little_endian]]

[@@@ocaml.warning "+32"]

(* ------------------- Misc ------------------- *)

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

let int_to_hex_string x size =
  Printf.sprintf "%x" x
  |> (fun s -> if (String.length s) < size
              then ((String.make (size -  String.length s) '0') ^ s)
              else if (String.length s) > size
              then failwith "int_to_hex_string: cannot allocate int into size"
               else s)
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
  let ()  = set_prefix_address (int_to_hex_string address 2) 0 pfx in
  let ()  = set_prefix_category (int_to_hex_string (category_to_int category) 2) 0 pfx in
  let ()  = set_prefix_setting (int_to_hex_string setting 2) 0 pfx in
  let ()  = set_prefix_index (int_to_hex_string 0 2) 0 pfx in
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

type response = Devinfo_fpga_ver   of int
              | Devinfo_hw_ver     of int
              | Devinfo_fw_ver     of float
              | Devinfo_serial     of int
              | Devinfo_type       of int
              | Overall_mode       of rw * e_overall_mode
              | Overall_app        of rw * e_overall_app
              | Overall_volatile   of rw * bool
              | Nw_ip              of rw * Ipaddr.V4.t
              | Nw_mask            of rw * Ipaddr.V4.t
              | Nw_gateway         of rw * Ipaddr.V4.t
              | Nw_dhcp            of rw * bool
              | Nw_reboot
              | Nw_mac             of Macaddr.t
              | Ip_method          of rw * e_nw_method
              | Ip_enable          of rw * bool
              | Ip_fec_delay       of int32
              | Ip_fec_enable      of rw * bool
              | Ip_fec_cols        of int
              | Ip_fec_rows        of int
              | Ip_jitter_tol      of int32
              | Ip_lost_after_fec  of int64
              | Ip_lost_before_fec of int64
              | Ip_udp_port        of rw * int
              | Ip_delay           of rw * int
              | Ip_mcast_addr      of rw * Ipaddr.V4.t
              | Ip_tp_per_ip       of int
              | Ip_status          of e_ip_status
              | Ip_protocol        of e_ip_protocol
              | Ip_packet_size     of e_ip_packet_size
              | Ip_bitrate         of int32
              | Ip_pcr_present     of bool
              | Ip_rate_change_cnt of int32
              | Ip_rate_estimation of rw * e_ip_rate_estimation
              | Ip_jitter_err_cnt  of int32
              | Ip_lock_err_cnt    of int32
              | Ip_delay_factor    of int32
              | Asi_packet_size    of rw * e_asi_packet_size
              | Asi_bitrate        of int32

type _ request = Get_devinfo_fpga_ver : response request
               | Get_devinfo_hw_ver   : response request
               | Get_devinfo_fw_ver   : response request
               | Get_devinfo_serial   : response request
               | Get_devinfo_type     : response request

let (detect : response request) = Get_devinfo_serial

let (init : response request list) = []

let probes = []

let period = 5

let serialize : type a. a request -> Cbuffer.t = function
  | Get_devinfo_fpga_ver -> to_empty_msg ~category:Devinfo ~setting:0x01 ~rw:Read ()
  | Get_devinfo_hw_ver   -> to_empty_msg ~category:Devinfo ~setting:0x02 ~rw:Read ()
  | Get_devinfo_fw_ver   -> to_empty_msg ~category:Devinfo ~setting:0x03 ~rw:Read ()
  | Get_devinfo_serial   -> to_empty_msg ~category:Devinfo ~setting:0x04 ~rw:Read ()
  | Get_devinfo_type     -> to_empty_msg ~category:Devinfo ~setting:0x05 ~rw:Read ()

(* ------------------- Requests/responses ------------------- *)

(* Overall *)

let to_req_set_overall_mode mode =
  let body = Cbuffer.create sizeof_setting8 in
  let ()   = set_setting8_data (e_overall_mode_to_int mode |> int_to_hex_string 1) 0 body in
  to_msg ~category:Overall ~setting:0x01 ~rw:Write ~body

let to_req_set_overall_application app =
  let body = Cbuffer.create sizeof_setting8 in
  let ()   = set_setting8_data (e_overall_app_to_int app |> int_to_hex_string 1) 0 body in
  to_msg ~category:Overall ~setting:0x02 ~rw:Write ~body

let to_req_set_overall_volatile enabled =
  let body = Cbuffer.create sizeof_setting8 in
  let ()   = set_setting8_data ((if enabled then 1 else 0) |> int_to_hex_string 1) 0 body in
  to_msg ~category:Overall ~setting:0x03 ~rw:Write ~body

(* Network *)

(* Deserialization *)

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
  let d = msg.data in
  match (msg.category, msg.setting) with
  | Devinfo, 0x01 -> Some (Devinfo_fpga_ver (Int64.to_int d))
  | Devinfo, 0x02 -> Some (Devinfo_hw_ver (Int64.to_int d))
  | Devinfo, 0x03 -> Some (Devinfo_fw_ver (Int64.to_float d /. 10.))
  | Devinfo, 0x04 -> Some (Devinfo_serial (Int64.to_int d))
  | Devinfo, 0x05 -> Some (Devinfo_type (Int64.to_int d))
  | _ -> None

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
