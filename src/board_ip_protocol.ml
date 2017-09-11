[@@@ocaml.warning "-32"]

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

(* ------------------- Misc ------------------- *)

let io = fun x -> Lwt_io.printf "%s\n" x |> ignore

type setting_size = I8 of int
                  | I16 of int
                  | I32 of int32
                  | I48 of int64
                  | I64 of int64

type devinfo_items =
  | Fpga_ver
  | Hw_ver
  | Fw_ver
  | Serial
  | Type [@@deriving to_yojson]

let devinfo_items_to_int = function
  | Fpga_ver -> 1 | Hw_ver -> 2 | Fw_ver -> 3 | Serial -> 4 | Type -> 5
let devinfo_items_of_int = function
  | 1 -> Some Fpga_ver | 2 -> Some Hw_ver | 3 -> Some Fw_ver
  | 4 -> Some Serial   | 5 -> Some Type   | _ -> None

type overall_items =
  | Mode
  | Application
  | Volatile [@@deriving to_yojson]

let overall_items_to_int = function
  | Mode -> 1 | Application -> 2 | Volatile -> 3
let overall_items_of_int = function
  | 1 -> Some Mode | 2 -> Some Application | 3 -> Some Volatile | _ -> None

type nw_items =
  | Ip
  | Mask
  | Gateway
  | Dhcp
  | Reboot
  | Mac [@@deriving to_yojson]

let nw_items_to_int = function
  | Ip -> 1 | Mask -> 2 | Gateway -> 3 | Dhcp -> 4 | Reboot -> 5 | Mac -> 6
let nw_items_of_int = function
  | 1 -> Some Ip     | 2 -> Some Mask | 3 -> Some Gateway | 4 -> Some Dhcp
  | 5 -> Some Reboot | 6 -> Some Mac  | _ -> None

type ip_items =
  | Method
  | Enable
  | Fec_delay
  | Fec_enable
  | Fec_cols
  | Fec_rows
  | Jitter_tol
  | Lost_after_fec
  | Lost_before_fec
  | Udp_port
  | Delay
  | Mcast_addr
  | Tp_per_ip
  | Status
  | Protocol
  | Output
  | Packet_size
  | Bitrate
  | PCR_present
  | Rate_change_cnt
  | Rate_est_mode
  | Jitter_err_cnt
  | Lock_err_cnt
  | Delay_factor [@@deriving to_yojson]

let ip_items_to_int = function
  | Method -> 1          | Enable -> 2          | Fec_delay -> 3     | Fec_enable -> 4
  | Fec_cols -> 5        | Fec_rows -> 6        | Jitter_tol -> 7    | Lost_after_fec -> 8
  | Lost_before_fec -> 9 | Udp_port -> 10       | Delay -> 11        | Mcast_addr -> 12
  | Tp_per_ip -> 13      | Status -> 14         | Protocol -> 15     | Output -> 17
  | Packet_size -> 18    | Bitrate -> 19        | PCR_present -> 20  | Rate_change_cnt -> 21
  | Rate_est_mode -> 22  | Jitter_err_cnt -> 23 | Lock_err_cnt -> 24 | Delay_factor -> 25
let ip_items_of_int = function
  | 1 -> Some Method          | 2 -> Some Enable          | 3 -> Some Fec_delay     | 4 -> Some Fec_enable
  | 5 -> Some Fec_cols        | 6 -> Some Fec_rows        | 7 -> Some Jitter_tol    | 8 -> Some Lost_after_fec
  | 9 -> Some Lost_before_fec | 10 -> Some Udp_port       | 11 -> Some Delay        | 12 -> Some Mcast_addr
  | 13 -> Some Tp_per_ip      | 14 -> Some Status         | 15 -> Some Protocol     | 17 -> Some Output
  | 18 -> Some Packet_size    | 19 -> Some Bitrate        | 20 -> Some PCR_present  | 21 -> Some Rate_change_cnt
  | 22 -> Some Rate_est_mode  | 23 -> Some Jitter_err_cnt | 24 -> Some Lock_err_cnt | 25 -> Some Delay_factor
  | _ -> None

type asi_items =
  | Packet_size
  | Bitrate [@@deriving to_yojson]

let asi_items_to_int = function
  | Packet_size -> 1 | Bitrate -> 3
let asi_items_of_int = function
  | 1 -> Some Packet_size | 3 -> Some Bitrate | _ -> None

type category = Devinfo of devinfo_items
              | Overall of overall_items
              | Nw of nw_items
              | Ip of ip_items
              | Asi of asi_items [@@deriving to_yojson]

type rw = Read | Write | Fail [@@deriving yojson]

type parsed =
  { cs   : category
  ; rw   : rw
  ; data : int64
  }

let stx = 0x02
let etx = 0x03
let address = 0x40

let cat_set_to_int_pair = function
  | Devinfo x -> 0x01, (devinfo_items_to_int x)
  | Overall x -> 0x02, (overall_items_to_int x)
  | Nw x      -> 0x03, (nw_items_to_int x)
  | Ip x      -> 0x81, (ip_items_to_int x)
  | Asi x     -> 0x84, (asi_items_to_int x)

let cat_set_of_int_pair = function
  | 0x01,x -> (match devinfo_items_of_int x with
              | Some s -> Some (Devinfo s)
              | None   -> None)
  | 0x02,x -> (match overall_items_of_int x with
              | Some s -> Some (Overall s)
              | None   -> None)
  | 0x03,x -> (match nw_items_of_int x with
              | Some s -> Some (Nw s)
              | None   -> None)
  | 0x81,x -> (match ip_items_of_int x with
              | Some s -> Some (Ip s)
              | None   -> None)
  | 0x84,x -> (match asi_items_of_int x with
              | Some s -> Some (Asi s)
              | None   -> None)
  | _      -> None

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

let has_index = function
  | Devinfo _ | Overall _ | Nw _ -> false
  | _                            -> true

let calc_crc msg =
  let _,body = Cbuffer.split msg 1 in
  let iter = Cbuffer.iter (fun _ -> Some 1)
                          (fun buf -> Cbuffer.get_uint8 buf 0)
                          body in
  Cbuffer.fold (fun acc el -> el + acc) iter 0
  |> fun x -> ((lnot x) + 1) land 0xFF

let to_prefix ~setting ~rw () =
  let c,s = cat_set_to_int_pair setting in
  let pfx = Cbuffer.create sizeof_prefix in
  let ()  = set_prefix_stx pfx stx in
  let ()  = set_prefix_address  (int_to_hex_string (I8 address)) 0 pfx in
  let ()  = set_prefix_category (int_to_hex_string (I8 c)) 0 pfx in
  let ()  = set_prefix_setting  (int_to_hex_string (I8 s)) 0 pfx in
  let ()  = set_prefix_index    (int_to_hex_string (I16 0)) 0 pfx in
  let ()  = set_prefix_rw pfx @@ rw_to_int rw in
  pfx

let to_suffix ~crc =
  let sfx = Cbuffer.create sizeof_suffix in
  let ()  = set_suffix_crc (int_to_hex_string (I8 crc)) 0 sfx in
  let ()  = set_suffix_etx sfx etx in
  sfx

let to_msg ~setting ~rw ~body () =
  let pfx = to_prefix ~setting ~rw () in
  let msg = Cbuffer.append pfx body in
  let sfx = to_suffix ~crc:(calc_crc msg) in
  Cbuffer.append msg sfx

let to_empty_msg ~setting ~rw =
  to_msg ~setting ~rw ~body:(Cbuffer.create 0)

(* ------------------- Board protocol implementation ------------------- *)

type instant = Board_meta.instant

type event = rw * category * int64 [@@deriving to_yojson]

type response = rw * category * int64 [@@deriving to_yojson]

type _ request = Get        : category               -> response request
               | Get_event  : category               -> event request
               | Set_int    : category * int         -> response request
               | Set_bool   : category * bool        -> response request
               | Set_ipaddr : category * Ipaddr.V4.t -> response request

let (detect : response request) = Get (Devinfo Fpga_ver)

let (init : response request list) = []

let probes = function
  | _ -> [ Get_event (Ip Udp_port)
         ; Get_event (Ip Mcast_addr)
         ; Get_event (Ip Tp_per_ip)
         ; Get_event (Ip Status) ]

let period = 5

type err = Bad_stx              of int
         | Bad_length           of int
         | Bad_address          of int
         | Bad_cat_set          of int * int
         | Bad_rw               of int
         | Bad_crc              of int * int
         | Insufficient_payload of Cbuffer.t
         | No_stx_after_msg
         | Unknown_err          of string

let string_of_err = function
  | Bad_stx x              -> "incorrect STX: "          ^ (string_of_int x)
  | Bad_length x           -> "incorrect length: "       ^ (string_of_int x)
  | Bad_address x          -> "incorrect address: "      ^ (string_of_int x)
  | Bad_cat_set (x,y)      -> (Printf.sprintf "incorrect category or/and setting: cat=%d, set=%d" x y)
  | Bad_rw x               -> "incorrect rw: "           ^ (string_of_int x)
  | Bad_crc (x,y)          -> "incorrect crc, expected " ^ (string_of_int x) ^ ", got " ^ (string_of_int y)
  | No_stx_after_msg       -> "no STX found after message payload"
  | Insufficient_payload _ -> "insufficient payload"
  | Unknown_err s          -> s

let get_setting_len cs =
  match cs with
  | Devinfo x -> (match x with
                  | Fpga_ver | Hw_ver        -> sizeof_setting8
                  | Fw_ver   | Serial | Type -> sizeof_setting32)
  | Overall _ -> sizeof_setting8
  | Nw      x -> (match x with
                  | Dhcp | Reboot           -> sizeof_setting8
                  | Ip   | Mask   | Gateway -> sizeof_setting32
                  | Mac                     -> sizeof_setting48)
  | Ip      x -> (match x with
                  | (Method   | Enable   | Fec_enable | Tp_per_ip
                     | Status | Protocol | Output     | Packet_size
                     | PCR_present       | Rate_est_mode)    -> sizeof_setting8
                  | Fec_cols | Fec_rows | Udp_port | Delay          -> sizeof_setting16
                  | (Fec_delay | Jitter_tol | Mcast_addr | Bitrate
                     | Rate_change_cnt | Jitter_err_cnt
                     | Lock_err_cnt | Delay_factor)                 -> sizeof_setting32
                  | Lost_after_fec | Lost_before_fec                -> sizeof_setting64)
  | Asi     x -> (match x with
                  | Packet_size -> sizeof_setting8
                  | Bitrate     -> sizeof_setting32)

let check_stx buf =
  let stx' = get_prefix_stx buf in
  if stx <> stx' then Error (Bad_stx stx') else Ok buf

let check_address buf =
  let address' = get_prefix_address buf |> int_of_ascii_buf in
  if address' <> address then Error (Bad_address address') else Ok buf

let check_rw buf =
  let rw' = get_prefix_rw buf in
  match rw_of_int rw' with
  | Some x -> Ok (buf,x)
  | None   -> Error (Bad_rw rw')

let check_rest (buf,rw)=
  let category' = get_prefix_category buf |> int_of_ascii_buf in
  let setting'  = get_prefix_setting buf  |> int_of_ascii_buf in
  match cat_set_of_int_pair (category',setting') with
  | Some cs -> let length   = get_setting_len cs in
               let pfx_len = if (has_index cs) then sizeof_prefix
                             else sizeof_prefix - sizeof_setting16 in
               let pfx,msg' = Cbuffer.split buf pfx_len in
               let body,sfx = Cbuffer.split msg' (Cbuffer.len msg' - (sizeof_suffix - 1)) in
               if (Cbuffer.len body) = length
               then let crc  = get_suffix_crc sfx |> int_of_ascii_buf in
                    let crc' = calc_crc (Cbuffer.append pfx body) in
                    if crc' <> crc
                    then Error (Bad_crc (crc', crc))
                    else Ok { cs ; rw ; data = match rw with
                                               | Fail -> 0L
                                               | _    -> int64_of_ascii_buf body
                            }
               else Error (Bad_length length)
  | None   -> Error (Bad_cat_set (category',setting'))

let get_msg buf =
  try
    CCResult.(check_stx buf
              >>= check_address
              >>= check_rw
              >>= check_rest)
  with e -> Error (Unknown_err (Printexc.to_string e))

let parse_msg : parsed -> response option = fun msg ->
  let rsp = (msg.rw, msg.cs, msg.data) in
  io (response_to_yojson rsp |> Yojson.Safe.to_string);
  Some rsp

let deserialize buf =
  let parts = Cbuffer.split_by_string (String.make 1 (char_of_int etx)) buf in
  let rec f = fun acc x -> match x with
                           | []       -> acc, None
                           | [x]      -> (match get_msg x with
                                          | Ok msg  -> (match parse_msg msg with
                                                        | Some x -> x :: acc, None
                                                        | None   -> acc, None)
                                          | Error e -> (match e with
                                                        | Insufficient_payload res -> io (string_of_err e); acc, Some res
                                                        | _                        -> io (string_of_err e); acc, None))
                           | hd :: tl -> (match get_msg hd with
                                          | Ok msg  -> (match parse_msg msg with
                                                        | Some x -> f (x::acc) tl
                                                        | None   -> f acc tl)
                                          | Error e -> io (string_of_err e); f acc tl) in
  let r,res = f [] parts in
  ([], r, res)

let to_req_get ~setting =
  to_empty_msg ~setting ~rw:Read

let to_req_set ~setting x =
  let body = pack_setting x in
  to_msg ~setting ~rw:Write ~body

let to_req_set_bool ~setting b =
  let body = pack_setting (I8 (if b then 1 else 0)) in
  to_msg ~setting ~rw:Write ~body

let to_req_set_ipaddr ~setting ip =
  let body = pack_setting (I32 (Ipaddr.V4.to_int32 ip)) in
  to_msg ~setting ~rw:Write ~body

let serialize : type a. a request -> Cbuffer.t = fun req ->
  match req with
    | Get c            -> to_req_get ~setting:c ()
    | Get_event c      -> to_req_get ~setting:c ()
    | Set_bool (c,b)   -> to_req_set_bool ~setting:c b ()
    | Set_int  (c,i)   -> let data = (match get_setting_len c with
                                      | x when x = sizeof_setting8  -> I8 i
                                      | x when x = sizeof_setting16 -> I16 i
                                      | x when x = sizeof_setting32 -> I32 (Int32.of_int i)
                                      | _ -> I16 i) in
                          to_req_set ~setting:c data ()
    | Set_ipaddr (c,i) -> to_req_set_ipaddr ~setting:c i ()

let is_response (type a) (req: a request) (resp:a) =
  match req,resp with
  | Get creq, (rw,crsp,_) when creq = crsp && (rw = Read || rw = Fail) -> Some resp
  | Get_event creq, (rw,crsp,_) when creq = crsp && (rw = Read || rw = Fail) -> Some resp
  | Set_int (creq,_), (rw,crsp,_) when creq = crsp && (rw = Write || rw = Fail) -> Some resp
  | Set_bool (creq,_), (rw,crsp,_) when creq = crsp && (rw = Write || rw = Fail) -> Some resp
  | Set_ipaddr (creq,_), (rw,crsp,_) when creq = crsp && (rw = Write || rw = Fail) -> Some resp
  | _ -> None

let pp _ = ()
