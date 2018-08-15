open Containers
open Board_types
include Board_msg_formats

(* -------------------- Requests/responses ----------------- *)

type event =
  | Fec_delay       of int
  | Fec_cols        of int
  | Fec_rows        of int
  | Jitter_tol      of int
  | Lost_after_fec  of int64
  | Lost_before_fec of int64
  | Tp_per_ip       of int
  | Status          of receiver_status
  | Protocol        of protocol
  | Packet_size     of packet_sz
  | Bitrate         of int
  | Pcr_present     of bool
  | Rate_change_cnt of int32
  | Jitter_err_cnt  of int32
  | Lock_err_cnt    of int32
  | Delay_factor    of int32
  | Asi_bitrate     of int [@@deriving show]

type _ devi =
  | Get_fpga_ver : int devi
  | Get_hw_ver   : int devi
  | Get_fw_ver   : int devi
  | Get_serial   : int devi
  | Get_type     : int devi

let mode_to_int = function
  | Asi2ip -> 0
  | Ip2asi -> 1
let mode_of_int = function
  | 0 -> Some Asi2ip
  | 1 -> Some Ip2asi
  | _ -> None
let application_to_int = function
  | Failsafe -> 0
  | Normal -> 1
let application_of_int = function
  | 0 -> Some Failsafe
  | 1 -> Some Normal
  | _ -> None
let storage_to_int = function
  | Flash -> 0
  | Ram -> 1
let storage_of_int = function
  | 0 -> Some Flash
  | 1 -> Some Ram
  | _ -> None

type _ overall =
  | Get_mode        : mode overall
  | Set_mode        : mode -> mode overall
  | Get_application : application overall
  | Set_application : application -> application overall
  | Get_storage     : storage overall
  | Set_storage     : storage -> storage overall

type _ nw =
  | Get_ip      : Ipaddr.V4.t nw
  | Get_mask    : Ipaddr.V4.t nw
  | Get_gateway : Ipaddr.V4.t nw
  | Get_dhcp    : bool nw
  | Get_mac     : Macaddr.t nw
  | Set_ip      : Ipaddr.V4.t -> Ipaddr.V4.t nw
  | Set_mask    : Ipaddr.V4.t -> Ipaddr.V4.t nw
  | Set_gateway : Ipaddr.V4.t -> Ipaddr.V4.t nw
  | Set_dhcp    : bool -> bool nw
  | Reboot      : unit nw

let meth_to_int = function
  | Unicast -> 0
  | Multicast -> 1
let meth_of_int = function
  | 0 -> Some Unicast
  | 1 -> Some Multicast
  | _ -> None
let rate_mode_to_int = function
  | On -> 0
  | Fixed -> 1
  | Without_pcr -> 2
  | Off -> 3
let rate_mode_of_int = function
  | 0 -> Some On
  | 1 -> Some Fixed
  | 2 -> Some Without_pcr
  | 3 -> Some Off
  | _ -> None
let status_of_int = function
  | 0 -> Some Enabled
  | 1 -> Some Disabled
  | 2 -> Some Failure
  | _ -> None
let protocol_of_int = function
  | 0 -> Some Udp
  | 1 -> Some Rtp
  | _ -> None
let output_of_int = function
  | 0 -> Some Asi
  | 1 -> Some Spi
  | _ -> None
let packet_sz_of_int = function
  | 0 -> Some Ts188
  | 1 -> Some Ts204
  | _ -> None

type _ ip =
  | Get_method           : meth ip
  | Set_method           : meth -> meth ip
  | Get_enable           : bool ip
  | Set_enable           : bool -> bool ip
  | Get_fec_delay        : event ip
  | Get_fec_enable       : bool ip
  | Set_fec_enable       : bool -> bool ip
  | Get_fec_cols         : event ip
  | Get_fec_rows         : event ip
  | Get_jitter_tol       : event ip
  | Get_lost_after_fec   : event ip
  | Get_lost_before_fec  : event ip
  | Get_udp_port         : int ip
  | Set_udp_port         : int -> int ip
  | Get_delay            : int ip
  | Set_delay            : int -> int ip
  | Get_mcast_addr       : Ipaddr.V4.t ip
  | Set_mcast_addr       : Ipaddr.V4.t -> Ipaddr.V4.t ip
  | Get_tp_per_ip        : event ip
  | Get_status           : event ip
  | Get_protocol         : event ip
  | Get_output           : output ip
  | Get_packet_size      : event ip
  | Get_bitrate          : event ip
  | Get_pcr_present      : event ip
  | Get_rate_change_cnt  : event ip
  | Get_rate_est_mode    : rate_mode ip
  | Set_rate_est_mode    : rate_mode -> rate_mode ip
  | Get_jitter_err_cnt   : event ip
  | Get_lock_err_cnt     : event ip
  | Get_delay_factor     : event ip

let asi_packet_sz_to_int = function
  | Sz Ts188 -> 0
  | Sz Ts204 -> 1
  | As_is -> 2
let asi_packet_sz_of_int = function
  | 0 -> Some (Sz Ts188)
  | 1 -> Some (Sz Ts204)
  | 2 -> Some (As_is)
  | _ -> None

type _ asi =
  | Get_packet_size : asi_packet_sz asi
  | Set_packet_size : asi_packet_sz -> asi_packet_sz asi
  | Get_bitrate     : event asi

type _ request =
  | Devinfo : 'a devi -> 'a request
  | Overall : 'a overall -> 'a request
  | Nw      : 'a nw -> 'a request
  | Ip      : 'a ip -> 'a request
  | Asi     : 'a asi -> 'a request

let request_to_cat_set : type a. a request -> int * int = function
  | Devinfo x ->
     0x01, begin match x with
           | Get_fpga_ver         -> 0x01
           | Get_hw_ver           -> 0x02
           | Get_fw_ver           -> 0x03
           | Get_serial           -> 0x04
           | Get_type             -> 0x05
           end
  | Overall x ->
     0x02, begin match x with
           | Get_mode             -> 0x01
           | Set_mode _           -> 0x01
           | Get_application      -> 0x02
           | Set_application _    -> 0x02
           | Get_storage          -> 0x03
           | Set_storage _        -> 0x03
           end
  | Nw x ->
     0x03, begin match x with
           | Get_ip               -> 0x01
           | Set_ip _             -> 0x01
           | Get_mask             -> 0x02
           | Set_mask _           -> 0x02
           | Get_gateway          -> 0x03
           | Set_gateway _        -> 0x03
           | Get_dhcp             -> 0x04
           | Set_dhcp _           -> 0x04
           | Reboot               -> 0x05
           | Get_mac              -> 0x06
           end
  | Ip x ->
     0x81, begin match x with
           | Get_method           -> 0x01
           | Set_method _         -> 0x01
           | Get_enable           -> 0x02
           | Set_enable _         -> 0x02
           | Get_fec_delay        -> 0x03
           | Get_fec_enable       -> 0x04
           | Set_fec_enable _     -> 0x04
           | Get_fec_cols         -> 0x05
           | Get_fec_rows         -> 0x06
           | Get_jitter_tol       -> 0x07
           | Get_lost_after_fec   -> 0x08
           | Get_lost_before_fec  -> 0x09
           | Get_udp_port         -> 0x0A
           | Set_udp_port _       -> 0x0A
           | Get_delay            -> 0x0B
           | Set_delay _          -> 0x0B
           | Get_mcast_addr       -> 0x0C
           | Set_mcast_addr _     -> 0x0C
           | Get_tp_per_ip        -> 0x0D
           | Get_status           -> 0x0E
           | Get_protocol         -> 0x0F
           | Get_output           -> 0x11
           | Get_packet_size      -> 0x12
           | Get_bitrate          -> 0x13
           | Get_pcr_present      -> 0x14
           | Get_rate_change_cnt  -> 0x15
           | Get_rate_est_mode    -> 0x16
           | Set_rate_est_mode _  -> 0x16
           | Get_jitter_err_cnt   -> 0x17
           | Get_lock_err_cnt     -> 0x18
           | Get_delay_factor     -> 0x19
           end
  | Asi x     ->
     0x84, begin match x with
           | Get_packet_size      -> 0x01
           | Set_packet_size _    -> 0x01
           | Get_bitrate          -> 0x03
           end

let cat_set_to_data_length = function
  (* Devinfo *)
  | 0x01, x ->
     begin match x with
     | 0x01 | 0x02 -> sizeof_setting8
     | _ -> sizeof_setting32
     end
  (* Overall *)
  | 0x02, _ -> sizeof_setting8
  | 0x03, x ->
     begin match x with
     | 0x04 -> sizeof_setting8
     | 0x05 -> 0
     | 0x06 -> sizeof_setting48
     | _ -> sizeof_setting32
     end
  | 0x81, x ->
     begin match x with
     | 0x03 | 0x07 | 0x0C |0x10 | 0x13 | 0x15 | 0x17 | 0x018 | 0x19 ->
        sizeof_setting32
     | 0x05 | 0x06 | 0x0A | 0x0B -> sizeof_setting16
     | 0x08 | 0x09 -> sizeof_setting64
     | _ -> sizeof_setting8
     end
  | 0x84, x ->
     begin match x with
     | 0x01 | 0x02 -> sizeof_setting8
     | _ -> sizeof_setting32
     end
  | _ -> assert false

(* ------------------- Misc ------------------- *)

let stx     = 0x02
let etx     = 0x03
let address = 0x40

type rw = Read | Write | Fail

type parsed =
  { category : int
  ; setting  : int
  ; rw       : rw
  ; body     : Cstruct.t
  ; rest     : Cstruct.t
  }

let rw_to_int = function
  | Read  -> int_of_char 'R'
  | Write -> int_of_char 'W'
  | Fail  -> int_of_char 'E'
let rw_of_int = function
  | x when (x = int_of_char 'R') || (x = int_of_char 'r') -> Some Read
  | x when (x = int_of_char 'W') || (x = int_of_char 'w') -> Some Write
  | x when (x = int_of_char 'E') || (x = int_of_char 'e') -> Some Fail
  | _ -> None

module type Getter = sig
  type t
  val get     : Cstruct.t -> t option
  val get_exn : Cstruct.t -> t
end

module Parse : sig
  module Int    : Getter with type t = int
  module Int32  : Getter with type t = int32
  module Int64  : Getter with type t = int64
  module Ipaddr : Getter with type t = Ipaddr.V4.t
  module Bool   : Getter with type t = bool
end = struct

  module Make(M:sig
                  type t
                  val of_string : string -> t option
                end) : (Getter with type t = M.t) = struct
    type t = M.t
    let get (b:Cstruct.t) : t option =
      let s = "0x" ^ (Cstruct.to_string b) in M.of_string s
    let get_exn : Cstruct.t -> t = Fun.(Option.get_exn % get)
  end

  module Int   = Make(Int)
  module Int32 = Make(Int32)
  module Int64 = Make(Int64)
  module Ipaddr : (Getter with type t = Ipaddr.V4.t) = struct
    type t = Ipaddr.V4.t
    let get b     = Option.map (fun x -> Ipaddr.V4.of_int32 x) @@ Int32.get b
    let get_exn b = Ipaddr.V4.of_int32 (Int32.get_exn b)
  end
  module Bool   : (Getter with type t = bool) = struct
    type t = bool
    let get b = Option.flat_map (function
                    | 0 -> Some false
                    | 1 -> Some true
                    | _ -> None) @@ Int.get b
    let get_exn b = Int.get_exn b
                    |> function
                      | 0 -> false
                      | 1 -> true
                      | _ -> failwith "bad bool value"
  end

end

module Request : sig
  val calc_crc : Cstruct.t -> int
  val make_get : 'a request -> Cstruct.t
  module Set : sig
    val bool : 'a request -> bool -> Cstruct.t
    val int8 : 'a request -> int -> Cstruct.t
    val int16 : 'a request -> int -> Cstruct.t
    val int32 : 'a request -> int32 -> Cstruct.t
    val ipaddr : 'a request -> Ipaddr.V4.t -> Cstruct.t
  end
end = struct

  let calc_crc msg =
    let _,body = Cstruct.split msg 1 in
    let iter   = Cstruct.iter (fun _ -> Some 1)
                   (fun buf -> Cstruct.get_uint8 buf 0)
                   body
    in Cstruct.fold (fun acc el -> el + acc) iter 0
       |> fun x -> ((lnot x) + 1) land 0xFF

  let to_hex_string x =
    let size,s = match x with
      | `I8 x  -> sizeof_setting8, Printf.sprintf "%X" x
      | `I16 x -> sizeof_setting16, Printf.sprintf "%X" x
      | `I32 x -> sizeof_setting32, Printf.sprintf "%lX" x
    in
    match String.length s with
    | len when len < size -> (String.make (size - len) '0') ^ s
    | len when len > size -> String.drop (len - size) s
    | _                   -> s

  let serialize x =
    let setter = match x with
      | `I8 _  -> set_setting8_data
      | `I16 _ -> set_setting16_data
      | `I32 _ -> set_setting32_data
    in
    let s   = to_hex_string x in
    let buf = Cstruct.create (String.length s) in
    let ()  = setter s 0 buf in
    buf

  let to_prefix ~request ~rw () =
    let c,s = request_to_cat_set request in
    let pfx = Cstruct.create sizeof_prefix in
    let ()  = set_prefix_stx pfx stx in
    let ()  = set_prefix_address  (to_hex_string (`I8 address)) 0 pfx in
    let ()  = set_prefix_category (to_hex_string (`I8 c)) 0 pfx in
    let ()  = set_prefix_setting  (to_hex_string (`I8 s)) 0 pfx in
    let ()  = set_prefix_rw pfx @@ rw_to_int rw in
    if (c >= 1 && c <= 3) then pfx
    else Cstruct.append pfx (serialize (`I16 0))

  let to_suffix ~crc =
    let sfx = Cstruct.create sizeof_suffix in
    let ()  = set_suffix_crc (to_hex_string (`I8 crc)) 0 sfx in
    let ()  = set_suffix_etx sfx etx in
    sfx

  let to_msg ~request ~rw ~body () =
    let pfx = to_prefix ~request ~rw () in
    let msg = Cstruct.append pfx body in
    let sfx = to_suffix ~crc:(calc_crc msg) in
    Cstruct.append msg sfx

  let to_empty_msg ~request ~rw =
    to_msg ~request ~rw ~body:(Cstruct.create 0)

  let make_get request = to_empty_msg ~request ~rw:Read ()

  module Set = struct
    let bool r x   =
      to_msg ~request:r
        ~rw:Write
        ~body:(serialize (`I8 (if x then 1 else 0))) ()
    let int8 r x   =
      to_msg ~request:r
        ~rw:Write
        ~body:(serialize (`I8 x)) ()
    let int16 r x  =
      to_msg ~request:r
        ~rw:Write
        ~body:(serialize (`I16 x)) ()
    let int32 r x  =
      to_msg ~request:r
        ~rw:Write
        ~body:(serialize (`I32 x)) ()
    let ipaddr r x =
      to_msg ~request:r
        ~rw:Write
        ~body:(serialize (`I32 (Ipaddr.V4.to_int32 x))) ()
  end

end

(* ----------------- Message deserialization ---------------- *)

module Make(Logs:Logs.LOG) = struct

  let sprintf = Printf.sprintf

  type err =
    | Bad_stx              of int
    | Bad_etx              of int
    | Bad_length           of int
    | Bad_address          of int
    | Bad_category         of int
    | Bad_setting          of int * int
    | Bad_rw               of int
    | Bad_crc              of int * int
    | Insufficient_payload of Cstruct.t
    | Unknown              of string

  let err_to_string = function
    | Bad_stx x -> sprintf "incorrect STX: %d" x
    | Bad_etx x -> sprintf "incorrect ETX: %d" x
    | Bad_length x -> sprintf "incorrect length: %d" x
    | Bad_address x -> sprintf "incorrect address: %d" x
    | Bad_category x -> sprintf "incorrect category: %d" x
    | Bad_setting (x, y) -> sprintf "incorrect setting: %d, in category %d" y x
    | Bad_rw x -> sprintf "incorrect rw: %d" x
    | Bad_crc (x, y) -> sprintf "incorrect crc, expected %d, got %d" x y
    | Insufficient_payload _ -> "insufficient payload"
    | Unknown s -> s

  let check_stx buf =
    let stx' = get_prefix_stx buf in
    if stx <> stx' then Error (Bad_stx stx') else Ok buf

  let check_address buf =
    let address' = get_prefix_address buf |> Parse.Int.get_exn in
    if address' <> address then Error (Bad_address address') else Ok buf

  let check_category buf =
    let category' = get_prefix_category buf |> Parse.Int.get_exn in
    match category' with
    | 0x01 | 0x02 | 0x03 | 0x81 | 0x84 -> Ok (category', buf)
    | _ -> Error (Bad_category category')

  let check_setting (cat,buf) =
    let setting' = get_prefix_setting buf |> Parse.Int.get_exn in
    match cat,setting' with
    | 0x01, x when x > 0 && x <= 0x05 -> Ok (cat, setting', buf)
    | 0x02, x when x > 0 && x <= 0x03 -> Ok (cat, setting', buf)
    | 0x03, x when x > 0 && x <= 0x06 -> Ok (cat, setting', buf)
    | 0x81, x when x > 0 && x <= 0x19 -> Ok (cat, setting', buf)
    | 0x84, x when x > 0 && x <= 0x03 -> Ok (cat, setting', buf)
    | _ -> Error (Bad_setting (cat,setting'))

  let check_rw (cat, set, buf) =
    let rw' = get_prefix_rw buf in
    match rw_of_int rw' with
    | Some x -> Ok (cat, set, x, buf)
    | None   -> Error (Bad_rw rw')

  let check_rest (cat, set, rw, buf) =
    let length = match rw with
      | Fail -> 0
      | _ -> cat_set_to_data_length (cat, set) in
    let pfx_len  =
      if (cat >= 1 && cat <= 3)
      then sizeof_prefix
      else sizeof_prefix + sizeof_setting16  in
    if Cstruct.len buf < (pfx_len + length + sizeof_suffix)
    then Error (Insufficient_payload buf)
    else (
      let pfx, msg' = Cstruct.split buf pfx_len in
      let body, rst' = Cstruct.split msg' length in
      let sfx, rest = Cstruct.split rst' sizeof_suffix in
      let crc' = get_suffix_crc sfx |> Parse.Int.get_exn in
      let etx' = get_suffix_etx sfx in
      let crc  = Request.calc_crc (Cstruct.append pfx body) in
      if      crc' <> crc then Error (Bad_crc (crc, crc'))
      else if etx' <> etx then Error (Bad_etx etx')
      else Ok { category = cat; setting = set; rw; body; rest })

  let get_msg buf =
    try
      Result.(
      check_stx buf
      >>= check_address
      >>= check_category
      >>= check_setting
      >>= check_rw
      >>= check_rest)
    with
    | Invalid_argument _ -> Error (Insufficient_payload buf)
    | e -> Error (Unknown (Printexc.to_string e))

  let deserialize buf =
    let parse x = match x.rw with
      | Read | Write -> `Ok x
      | Fail ->
         Logs.warn (fun m -> m "got error in respose: cat = %d, set = %d"
                               x.category x.setting);
         `Error x in
    let rec f responses b =
      if Cstruct.len b > (sizeof_prefix + sizeof_suffix)
      then
        match get_msg b with
        | Ok x -> f ((parse x) :: responses) x.rest
        | Error e ->
           begin match e with
           | Insufficient_payload x -> List.rev responses, x
           | e -> Logs.warn (fun m -> m "parser error: %s" @@ err_to_string e);
                  f responses (Cstruct.shift b 1)
           end
      else List.rev responses, b in
    let r, res = f [] buf
    in
    List.rev r, if Cstruct.len res > 0 then Some res else None

end

let is_response (type a) (req : a request) m : a option =
  let open Parse in
  let c, s = request_to_cat_set req in
  match m with
  | `Ok { category; setting; body = b; _ }
       when c = category && s = setting ->
     begin match req with
     | Devinfo x ->
        begin match x with
        | Get_fpga_ver -> Int.get b
        | Get_hw_ver -> Int.get b
        | Get_fw_ver -> Int.get b
        | Get_serial -> Int.get b
        | Get_type -> Int.get b
        end
     | Overall x ->
        let open Option.Infix in
        begin match x with
        | Get_mode -> Int.get b >>= mode_of_int
        | Get_application -> Int.get b >>= application_of_int
        | Get_storage -> Int.get b >>= storage_of_int
        | Set_mode _ -> Int.get b >>= mode_of_int
        | Set_application _ -> Int.get b >>= application_of_int
        | Set_storage _ -> Int.get b >>= storage_of_int
        end
     | Nw x ->
        begin match x with
        | Get_ip -> Ipaddr.get b
        | Get_mask -> Ipaddr.get b
        | Get_gateway -> Ipaddr.get b
        | Get_dhcp -> Bool.get b
        | Get_mac -> let rec f = fun acc s ->
                       match String.take_drop 2 s with
                       | (x,"")  -> (acc ^ x)
                       | (x,res) -> f (acc ^ x ^ ":") res in
                     Macaddr.of_string (f "" (Cstruct.to_string b))
        | Set_ip _ -> Ipaddr.get b
        | Set_mask _ -> Ipaddr.get b
        | Set_gateway _ -> Ipaddr.get b
        | Set_dhcp _ -> Bool.get b
        | Reboot -> Some ()
        end
     | Ip x ->
        let open Option.Infix in
        begin match x with
        | Get_enable          -> Bool.get b
        | Get_fec_enable      -> Bool.get b
        | Get_pcr_present     -> Bool.get b  >|= (fun x -> Pcr_present x)
        | Get_method          -> Int.get b   >>= meth_of_int
        | Get_fec_delay       -> Int.get b   >|= (fun x -> Fec_delay x)
        | Get_fec_cols        -> Int.get b   >|= (fun x -> Fec_cols x)
        | Get_fec_rows        -> Int.get b   >|= (fun x -> Fec_rows x)
        | Get_jitter_tol      -> Int.get b   >|= (fun x -> Jitter_tol x)
        | Get_udp_port        -> Int.get b
        | Get_delay           -> Int.get b
        | Get_tp_per_ip       -> Int.get b   >|= (fun x -> Tp_per_ip x)
        | Get_status          -> Int.get b   >>= status_of_int    >|= (fun x -> Status x)
        | Get_protocol        -> Int.get b   >>= protocol_of_int  >|= (fun x -> Protocol x)
        | Get_output          -> Int.get b   >>= output_of_int
        | Get_packet_size     -> Int.get b   >>= packet_sz_of_int >|= (fun x -> Packet_size x)
        | Get_bitrate         -> Int.get b   >|= (fun x -> Bitrate x)
        | Get_rate_est_mode   -> Int.get b   >>= rate_mode_of_int
        | Get_rate_change_cnt -> Int32.get b >|= (fun x -> Rate_change_cnt x)
        | Get_jitter_err_cnt  -> Int32.get b >|= (fun x -> Jitter_err_cnt x)
        | Get_lock_err_cnt    -> Int32.get b >|= (fun x -> Lock_err_cnt x)
        | Get_delay_factor    -> Int32.get b >|= (fun x -> Delay_factor x)
        | Get_lost_after_fec  -> Int64.get b >|= (fun x -> Lost_after_fec x)
        | Get_lost_before_fec -> Int64.get b >|= (fun x -> Lost_before_fec x)
        | Get_mcast_addr      -> Ipaddr.get b
        (* Setters *)
        | Set_enable _        -> Bool.get b
        | Set_fec_enable _    -> Bool.get b
        | Set_method _        -> Int.get b   >>= meth_of_int
        | Set_udp_port _      -> Int.get b
        | Set_delay _         -> Int.get b
        | Set_rate_est_mode _ -> Int.get b   >>= rate_mode_of_int
        | Set_mcast_addr _    -> Ipaddr.get b
        end
     | Asi x ->
        let open Option.Infix in
        begin match x with
        | Get_packet_size -> Int.get b   >>= asi_packet_sz_of_int
        | Get_bitrate -> Int.get b   >|= (fun x -> Asi_bitrate x)
        | Set_packet_size _ -> Int.get b   >>= asi_packet_sz_of_int
        end
     end
  | `Ok _    -> None
  | `Error _ -> None
  | _        -> None
