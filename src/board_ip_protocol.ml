[@@@ocaml.warning "-32"]

[%%cstruct
 type prefix =
   { tag      : uint8_t
   ; address  : uint16_t
   ; category : uint16_t
   ; setting  : uint16_t
   ; rw       : uint8_t
   ; index    : uint32_t
   } [@@little_endian]]

[%%cstruct
 type suffix =
   { crc : uint16_t
   ; tag : uint8_t
   } [@@little_endian]]

[@@@ocaml.warning "+32"]

(* ------------------- Misc ------------------- *)

let start_tag = 0x02
let stop_tag = 0x03
let address = 0x40

type rw = Read | Write
let rw_to_int = function
  | Read  -> int_of_char 'R'
  | Write -> int_of_char 'W'
let rw_of_int = function
  | x when x = int_of_char 'R' -> Some Read
  | x when x = int_of_char 'W' -> Some Write
  | _                          -> None

let calc_crc _ = 0

let to_prefix ~category ~setting ~rw () =
  let pfx = Cbuffer.create sizeof_prefix in
  let ()  = set_prefix_tag pfx start_tag in
  let ()  = set_prefix_address pfx address in
  let ()  = set_prefix_category pfx category in
  let ()  = set_prefix_setting pfx setting in
  let ()  = set_prefix_rw pfx @@ rw_to_int rw in
  pfx

let to_suffix ~crc =
  let sfx = Cbuffer.create sizeof_suffix in
  let ()  = set_suffix_crc sfx crc in
  let ()  = set_suffix_tag sfx stop_tag in
  sfx

let to_msg ~category ~setting ~rw ~body =
  let pfx = to_prefix ~category ~setting ~rw () in
  let msg = Cbuffer.append pfx body in
  let sfx = to_suffix ~crc:(calc_crc msg) in
  Cbuffer.append msg sfx


