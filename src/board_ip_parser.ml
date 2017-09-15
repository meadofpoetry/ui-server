[@@@ocaml.warning "-32"]

[%%cstruct
 type prefix =
   { stx      : uint8_t
   ; address  : uint8_t [@len 2]
   ; category : uint8_t [@len 2]
   ; setting  : uint8_t [@len 2]
   ; rw       : uint8_t
   } [@@little_endian]]

[%%cstruct
 type suffix =
   { crc : uint8_t [@len 2]
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

open Common.Board.Ip

(* -------------------- Requests/responses ----------------- *)

type _ devinfo =
  | Get_fpga_ver : int devinfo
  | Get_hw_ver   : int devinfo
  | Get_fw_ver   : int devinfo
  | Get_serial   : int devinfo
  | Get_type     : int devinfo

let mode_to_int = function
  | Asi2ip -> 0 | Ip2asi -> 1
let mode_of_int = function
  | 0 -> Some Asi2ip | 1 -> Some Ip2asi | _ -> None
let application_to_int = function
  | Failsafe -> 0 | Normal -> 1
let application_of_int = function
  | 0 -> Some Failsafe | 1 -> Some Normal | _ -> None
let storage_to_int = function
  | Flash -> 0 | Ram -> 1
let storage_of_int = function
  | 0 -> Some Flash | 1 -> Some Ram | _ -> None

type _ overall =
  | Get_mode        : mode overall
  | Set_mode        : mode -> mode overall
  | Get_application : application overall
  | Set_application : application -> application overall
  | Get_storage     : storage overall
  | Set_storage     : storage -> storage overall

type _ nw =
  | Get_ip      : Ipaddr.V4.t nw
  | Set_ip      : Ipaddr.V4.t -> Ipaddr.V4.t nw
  | Get_mask    : Ipaddr.V4.t nw
  | Set_mask    : Ipaddr.V4.t -> Ipaddr.V4.t nw
  | Get_gateway : Ipaddr.V4.t nw
  | Set_gateway : Ipaddr.V4.t -> Ipaddr.V4.t nw
  | Get_dhcp    : bool nw
  | Set_dhcp    : bool -> bool nw
  | Reboot      : unit nw
  | Get_mac     : Macaddr.t nw

type meth      = Unicast | Multicast

let meth_to_int = function
  | Unicast -> 0 | Multicast -> 1
let meth_of_int = function
  | 0 -> Some Unicast | 1 -> Some Multicast | _ -> None
let rate_mode_to_int = function
  | On -> 0 | Fixed -> 1 | Without_pcr -> 2 | Off -> 3
let rate_mode_of_int = function
  | 0 -> Some On | 1 -> Some Fixed | 2 -> Some Without_pcr | 3 -> Some Off | _ -> None
let status_of_int = function
  | 0 -> Some Enabled | 1 -> Some Disabled | 2 -> Some Failure | _ -> None
let protocol_of_int = function
  | 0 -> Some Udp | 1 -> Some Rtp | _ -> None
let output_of_int = function
  | 0 -> Some Asi | 1 -> Some Spi | _ -> None
let packet_sz_of_int = function
  | 0 -> Some Ts188 | 1 -> Some Ts204 | _ -> None

type _ ip =
  | Get_method           : meth ip
  | Set_method           : meth -> meth ip
  | Get_enable           : bool ip
  | Set_enable           : bool -> bool ip
  | Get_fec_enable       : bool ip
  | Set_fec_enable       : bool -> bool ip
  | Get_udp_port         : int ip
  | Set_udp_port         : int -> int ip
  | Get_delay            : int ip
  | Set_delay            : int -> int ip
  | Get_mcast_addr       : Ipaddr.V4.t ip
  | Set_mcast_addr       : Ipaddr.V4.t -> Ipaddr.V4.t ip
  | Get_rate_est_mode    : rate_mode ip
  | Set_rate_est_mode    : rate_mode -> rate_mode ip

let asi_packet_sz_to_int = function
  | Sz Ts188 -> 0 | Sz Ts204 -> 1 | As_is -> 2
let asi_packet_sz_of_int = function
  | 0 -> Some (Sz Ts188) | 1 -> Some (Sz Ts204) | 2 -> Some (As_is) | _ -> None

type _ asi =
  | Get_packet_size : asi_packet_sz asi
  | Set_packet_size : asi_packet_sz -> asi_packet_sz asi
  | Get_bitrate     : int32 asi

type 'a request = Devinfo of 'a devinfo
                | Overall of 'a overall
                | Nw      of 'a nw
                | Ip      of 'a ip
                | Asi     of 'a asi

type _ event_request = Get_fec_delay        : int event_request
                     | Get_fec_cols         : int event_request
                     | Get_fec_rows         : int event_request
                     | Get_jitter_tol       : int event_request
                     | Get_lost_after_fec   : int64 event_request
                     | Get_lost_before_fec  : int64 event_request
                     | Get_tp_per_ip        : int event_request
                     | Get_status           : status event_request
                     | Get_protocol         : protocol event_request
                     | Get_packet_size      : packet_sz event_request
                     | Get_bitrate          : int event_request
                     | Get_pcr_present      : bool event_request
                     | Get_rate_change_cnt  : int32 event_request
                     | Get_jitter_err_cnt   : int32 event_request
                     | Get_lock_err_cnt     : int32 event_request
                     | Get_delay_factor     : int32 event_request

let request_to_cat_set : type a. a request -> int * int = function
  | Devinfo x -> 0x01, (match x with
                        | Get_fpga_ver -> 0x01
                        | Get_hw_ver   -> 0x02
                        | Get_fw_ver   -> 0x03
                        | Get_serial   -> 0x04
                        | Get_type     -> 0x05)
  | Overall x -> 0x02, (match x with
                        | Get_mode          -> 0x01
                        | Set_mode _        -> 0x01
                        | Get_application   -> 0x02
                        | Set_application _ -> 0x02
                        | Get_storage       -> 0x03
                        | Set_storage _     -> 0x03)
  | Nw x      -> 0x03, (match x with
                        | Get_ip        -> 0x01
                        | Set_ip _      -> 0x01
                        | Get_mask      -> 0x02
                        | Set_mask _    -> 0x02
                        | Get_gateway   -> 0x03
                        | Set_gateway _ -> 0x03
                        | Get_dhcp      -> 0x04
                        | Set_dhcp _    -> 0x04
                        | Reboot        -> 0x05
                        | Get_mac       -> 0x06)
  | Ip x      -> 0x81, (match x with
                        | Get_method           -> 0x01
                        | Set_method _         -> 0x01
                        | Get_enable           -> 0x02
                        | Set_enable _         -> 0x02
                        | Get_fec_enable       -> 0x04
                        | Set_fec_enable _     -> 0x04
                        | Get_udp_port         -> 0x0A
                        | Set_udp_port _       -> 0x0A
                        | Get_delay            -> 0x0B
                        | Set_delay _          -> 0x0B
                        | Get_mcast_addr       -> 0x0C
                        | Set_mcast_addr _     -> 0x0C
                        | Get_rate_est_mode    -> 0x16
                        | Set_rate_est_mode _  -> 0x16)
  | Asi x     -> 0x84, (match x with
                       | Get_packet_size   -> 0x01
                       | Set_packet_size _ -> 0x01
                       | Get_bitrate       -> 0x03)

let event_request_to_cat_set : type a. a event_request -> int * int = function
  | Get_fec_delay       -> 0x81,0x03
  | Get_fec_cols        -> 0x81,0x05
  | Get_fec_rows        -> 0x81,0x06
  | Get_jitter_tol      -> 0x81,0x07
  | Get_lost_after_fec  -> 0x81,0x08
  | Get_lost_before_fec -> 0x81,0x09
  | Get_tp_per_ip       -> 0x81,0x0D
  | Get_status          -> 0x81,0x0E
  | Get_protocol        -> 0x81,0x0F
  | Get_packet_size     -> 0x81,0x12
  | Get_bitrate         -> 0x81,0x13
  | Get_pcr_present     -> 0x81,0x14
  | Get_rate_change_cnt -> 0x81,0x15
  | Get_jitter_err_cnt  -> 0x81,0x17
  | Get_lock_err_cnt    -> 0x81,0x18
  | Get_delay_factor    -> 0x81,0x19

let cat_set_to_data_length = function
  | 0x01,x -> (match x with                         (* Devinfo *)
               | 0x01 | 0x02 -> sizeof_setting8
               | _           -> sizeof_setting32)
  | 0x02,_ -> sizeof_setting8                       (* Overall *)
  | 0x03,x -> (match x with
               | 0x04        -> sizeof_setting8
               | 0x05        -> 0
               | 0x06        -> sizeof_setting48
               | _           -> sizeof_setting32)
  | 0x81,x -> (match x with
               | 0x03 | 0x07 | 0x10 | 0x13 | 0x15 | 0x16 | 0x17 | 0x018 | 0x19 -> sizeof_setting32
               | 0x05 | 0x06 | 0x0A | 0x0B                                     -> sizeof_setting16
               | 0x08 | 0x09                                                   -> sizeof_setting64
               | _                                                             -> sizeof_setting8)
  | 0x84,x -> (match x with
               | 0x01 | 0x02 -> sizeof_setting8
               | _           -> sizeof_setting32)
  | _      -> assert false

(* ------------------- Misc ------------------- *)

let stx = 0x02
let etx = 0x03
let address = 0x40

type rw = Read | Write | Fail
let rw_to_int = function
  | Read  -> int_of_char 'R'
  | Write -> int_of_char 'W'
  | Fail  -> int_of_char 'E'
let rw_of_int = function
  | x when (x = int_of_char 'R') || (x = int_of_char 'r') -> Some Read
  | x when (x = int_of_char 'W') || (x = int_of_char 'w') -> Some Write
  | x when (x = int_of_char 'E') || (x = int_of_char 'e') -> Some Fail
  | _ -> None

let io = fun x -> Lwt_io.printf "%s\n" x |> ignore

let to_hex_string x =
  let size,s = (match x with
                | `I8 x  -> sizeof_setting8, Printf.sprintf "%X" x
                | `I16 x -> sizeof_setting16, Printf.sprintf "%X" x
                | `I32 x -> sizeof_setting32, Printf.sprintf "%lX" x) in
  let s_sz = CCString.length s in
  if s_sz < size        then ((CCString.make (size -  s_sz) '0') ^ s)
  else if (s_sz > size) then CCString.drop (s_sz - size) s
  else s

let to_cbuffer x =
  let f = (match x with
           | `I8 _ -> set_setting8_data
           | `I16 _ -> set_setting16_data
           | `I32 _ -> set_setting32_data) in
  let hs = to_hex_string x in
  Cbuffer.create (String.length hs) |> fun b -> f hs 0 b; b

let try_parse f x             = try Some (f x) with _ -> None
let hex_string_of_ascii_buf x = "0x" ^ (Cbuffer.to_string x)
let parse_int_exn x           = hex_string_of_ascii_buf x |> int_of_string
let parse_int32_exn x         = hex_string_of_ascii_buf x |> Int32.of_string
let parse_int64_exn x         = hex_string_of_ascii_buf x |> Int64.of_string
let parse_ipaddr_exn x        = parse_int32_exn x |> Ipaddr.V4.of_int32
let parse_bool_exn x          = parse_int_exn x
                                |> function | 0 -> false | 1 -> true | _ -> failwith "bad bool value"
let parse_int                 = try_parse parse_int_exn
let parse_int32               = try_parse parse_int32_exn
let parse_int64               = try_parse parse_int64_exn
let parse_ipaddr              = try_parse parse_ipaddr_exn
let parse_bool                = try_parse parse_bool_exn

(* -------------------- Message constructors ------------------*)

let calc_crc msg =
  let _,body = Cbuffer.split msg 1 in
  let iter = Cbuffer.iter (fun _ -> Some 1)
                          (fun buf -> Cbuffer.get_uint8 buf 0)
                          body in
  Cbuffer.fold (fun acc el -> el + acc) iter 0
  |> fun x -> ((lnot x) + 1) land 0xFF

let to_prefix (c,s) ~rw () =
  let pfx = Cbuffer.create sizeof_prefix in
  let ()  = set_prefix_stx pfx stx in
  let ()  = set_prefix_address  (to_hex_string (`I8 address)) 0 pfx in
  let ()  = set_prefix_category (to_hex_string (`I8 c)) 0 pfx in
  let ()  = set_prefix_setting  (to_hex_string (`I8 s)) 0 pfx in
  (* let ()  = set_prefix_index    (to_hex_string (`I16 0)) 0 pfx in *)
  let ()  = set_prefix_rw pfx @@ rw_to_int rw in
  if (c >= 1 && c <= 3) then pfx
  else Cbuffer.append pfx (to_cbuffer (`I16 0))

let to_suffix ~crc =
  let sfx = Cbuffer.create sizeof_suffix in
  let ()  = set_suffix_crc (to_hex_string (`I8 crc)) 0 sfx in
  let ()  = set_suffix_etx sfx etx in
  sfx

let to_msg ~request ~rw ~body () =
  let pfx = to_prefix (request_to_cat_set request) ~rw () in
  let msg = Cbuffer.append pfx body in
  let sfx = to_suffix ~crc:(calc_crc msg) in
  Cbuffer.append msg sfx

let to_empty_msg ~request ~rw =
  to_msg ~request ~rw ~body:(Cbuffer.create 0)

let to_req_get request =
  to_empty_msg ~request ~rw:Read ()
  |> fun x -> io ("Get request: " ^ Cbuffer.pp x); x

let to_req_set_bool request b =
  to_msg ~request ~rw:Write ~body:(to_cbuffer (`I8 (if b then 1 else 0))) ()

let to_req_set_int8 request x =
  to_msg ~request ~rw:Write ~body:(to_cbuffer (`I8 x)) ()

let to_req_set_int16 request x =
  to_msg ~request ~rw:Write ~body:(to_cbuffer (`I16 x)) ()

let to_req_set_int32 request x =
  to_msg ~request ~rw:Write ~body:(to_cbuffer (`I32 x)) ()

let to_req_set_ipaddr request ip =
  to_msg ~request ~rw:Write ~body:(to_cbuffer (`I32 (Ipaddr.V4.to_int32 ip))) ()

let to_req_get_event ~event ~rw () =
  let pfx = to_prefix (event_request_to_cat_set event) ~rw () in
  let sfx = to_suffix ~crc:(calc_crc pfx) in
  Cbuffer.append pfx sfx

(* ----------------- Message deserialization ---------------- *)

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
  | Bad_setting (x,y)      -> "incorrect setting: "      ^ (string_of_int y) ^ ", in category " ^ (string_of_int x)
  | Bad_rw x               -> "incorrect rw: "           ^ (string_of_int x)
  | Bad_crc (x,y)          -> "incorrect crc, expected " ^ (string_of_int x) ^ ", got " ^ (string_of_int y)
  | No_stx_after_msg       -> "no STX found after message payload"
  | Insufficient_payload _ -> "insufficient payload"
  | Unknown_err s          -> s

let check_stx buf =
  let stx' = get_prefix_stx buf in
  if stx <> stx' then Error (Bad_stx stx') else Ok buf

let check_address buf =
  let address' = get_prefix_address buf |> parse_int_exn in
  if address' <> address then Error (Bad_address address') else Ok buf

let check_category buf =
  let category' = get_prefix_category buf |> parse_int_exn in
  match category' with
  | 0x01 | 0x02 | 0x03 | 0x81 | 0x84 -> Ok (category',buf)
  | _    -> Error (Bad_category category')

let check_setting (cat,buf) =
  let setting' = get_prefix_setting buf |> parse_int_exn in
  match cat,setting' with
  | 0x01, x when x > 0 && x <= 0x05 -> Ok (cat,setting',buf)
  | 0x02, x when x > 0 && x <= 0x03 -> Ok (cat,setting',buf)
  | 0x03, x when x > 0 && x <= 0x06 -> Ok (cat,setting',buf)
  | 0x81, x when x > 0 && x <= 0x19 -> Ok (cat,setting',buf)
  | 0x84, x when x > 0 && x <= 0x03 -> Ok (cat,setting',buf)
  | _                               -> Error (Bad_setting (cat,setting'))

let check_rw (cat,set,buf) =
  let rw' = get_prefix_rw buf in
  match rw_of_int rw' with
  | Some x -> Ok (cat,set,x,buf)
  | None   -> Error (Bad_rw rw')

let check_rest (cat,set,rw,buf) =
  let length   = (match rw with
                  | Fail -> 0
                  | _    -> cat_set_to_data_length (cat,set)) in
  let pfx_len  = if (cat >= 1 && cat <= 3) then sizeof_prefix
                 else sizeof_prefix + sizeof_setting16 in
  let pfx,msg' = Cbuffer.split buf pfx_len in
  let body,sfx = Cbuffer.split msg' length in
  let crc      = get_suffix_crc sfx |> parse_int_exn in
  let crc'     = calc_crc (Cbuffer.append pfx body) in
  if crc' <> crc then Error (Bad_crc (crc', crc))
  else Ok (cat,set,rw,body)

let get_msg buf =
  try
    CCResult.(check_stx buf
              >>= check_address
              >>= check_category
              >>= check_setting
              >>= check_rw
              >>= check_rest)
  with
  | Invalid_argument _ -> Error (Insufficient_payload buf)
  | e -> Error (Unknown_err (Printexc.to_string e))

let deserialize buf =
  let parse = fun ((_,_,rw,_) as x) ->
    match rw with
    | Read | Write -> `Ok x
    | Fail         -> `Error x in
  let parts = Cbuffer.split_by_string (String.make 1 (char_of_int etx)) buf in
  let rec f =
    fun acc x -> match x with
                 | []       -> acc, None
                 | [x]      -> (match get_msg x with
                                | Ok msg  -> (parse msg) :: acc, None
                                | Error e -> io (string_of_err e);
                                             (match e with
                                              | Insufficient_payload res -> acc, Some res
                                              | _                        -> acc, None))
                 | hd :: tl -> (match get_msg hd with
                                | Ok msg  -> f ((parse msg) :: acc) tl
                                | Error e -> io (string_of_err e); f acc tl) in
  let r,res = f [] parts in (List.rev r, res)

let is_response (type a) (req : a request) m : a option =
  match m with
  | `Ok (cat,set,_,b) ->
     let c,s = request_to_cat_set req in
     if c <> cat || s <> set then None
     else (match req with
           | Devinfo x -> let i = parse_int b in
                          (match x with
                           | Get_fpga_ver -> i
                           | Get_hw_ver   -> i
                           | Get_fw_ver   -> i
                           | Get_serial   -> i
                           | Get_type     -> i)
           | Overall x -> let i = parse_int b in
                          let open CCOpt.Infix in
                          (match x with
                           | Get_mode          -> i >>= mode_of_int
                           | Get_application   -> i >>= application_of_int
                           | Get_storage       -> i >>= storage_of_int
                           | Set_mode _        -> i >>= mode_of_int
                           | Set_application _ -> i >>= application_of_int
                           | Set_storage _     -> i >>= storage_of_int)
           | Nw x      -> (match x with
                           | Get_ip        -> parse_ipaddr b
                           | Get_mask      -> parse_ipaddr b
                           | Get_gateway   -> parse_ipaddr b
                           | Get_dhcp      -> parse_bool b
                           | Get_mac       -> let rec f = fun acc s ->
                                                match CCString.take_drop 2 s with
                                                | (x,"")  -> (acc ^ x)
                                                | (x,res) -> f (acc ^ x ^ ":") res in
                                              Macaddr.of_string (f "" (Cbuffer.to_string b))
                           | Set_ip _      -> parse_ipaddr b
                           | Set_mask _    -> parse_ipaddr b
                           | Set_gateway _ -> parse_ipaddr b
                           | Set_dhcp _    -> parse_bool b
                           | Reboot        -> Some ())
           | Ip x      -> (match x with
                           | Get_method          -> CCOpt.(parse_int b >>= meth_of_int)
                           | Get_enable          -> parse_bool b
                           | Get_fec_enable      -> parse_bool b
                           | Get_udp_port        -> parse_int b
                           | Get_delay           -> parse_int b
                           | Get_mcast_addr      -> parse_ipaddr b
                           | Get_rate_est_mode   -> CCOpt.(parse_int b >>= rate_mode_of_int)
                           | Set_method _        -> CCOpt.(parse_int b >>= meth_of_int)
                           | Set_enable _        -> parse_bool b
                           | Set_fec_enable _    -> parse_bool b
                           | Set_udp_port _      -> parse_int b
                           | Set_delay _         -> parse_int b
                           | Set_mcast_addr _    -> parse_ipaddr b
                           | Set_rate_est_mode _ -> CCOpt.(parse_int b >>= rate_mode_of_int))
           | Asi x     -> (match x with
                           | Get_packet_size   -> CCOpt.(parse_int b >>= asi_packet_sz_of_int)
                           | Get_bitrate       -> parse_int32 b
                           | Set_packet_size _ -> CCOpt.(parse_int b >>= asi_packet_sz_of_int)))
  | `Error _ -> None
  | _        -> None

let is_event (type a) (req : a event_request) m : a option =
  match m with
  | `Ok (cat,set,_,b) ->
     let c,s = event_request_to_cat_set req in
     if c <> cat || s <> set then None
     else (match req with
           | Get_fec_delay       -> parse_int b
           | Get_fec_cols        -> parse_int b
           | Get_fec_rows        -> parse_int b
           | Get_jitter_tol      -> parse_int b
           | Get_lost_after_fec  -> parse_int64 b
           | Get_lost_before_fec -> parse_int64 b
           | Get_tp_per_ip       -> parse_int b
           | Get_status          -> CCOpt.(parse_int b >>= status_of_int)
           | Get_protocol        -> CCOpt.(parse_int b >>= protocol_of_int)
           | Get_packet_size     -> CCOpt.(parse_int b >>= packet_sz_of_int)
           | Get_bitrate         -> parse_int b
           | Get_pcr_present     -> parse_bool b
           | Get_rate_change_cnt -> parse_int32 b
           | Get_jitter_err_cnt  -> parse_int32 b
           | Get_lock_err_cnt    -> parse_int32 b
           | Get_delay_factor    -> parse_int32 b)
  | `Error _ -> None
  | _        -> None
