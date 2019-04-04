open Request
open Message

let calc_crc msg =
  let _, body = Cstruct.split msg 1 in
  let iter =
    Cstruct.iter (fun _ -> Some 1)
      (fun buf -> Cstruct.get_uint8 buf 0)
      body in
  let sum = Cstruct.fold (+) iter 0 in
  ((lnot sum) + 1) land 0xFF

let to_hex_string x =
  let size, s = match x with
    | `I8 x -> sizeof_setting8, Printf.sprintf "%X" x
    | `I16 x -> sizeof_setting16, Printf.sprintf "%X" x
    | `I32 x -> sizeof_setting32, Printf.sprintf "%lX" x in
  match String.length s with
  | len when len < size -> (String.make (size - len) '0') ^ s
  | len when len > size -> String.drop (len - size) s
  | _ -> s

let serialize x =
  let setter = match x with
    | `I8 _ -> set_setting8_data
    | `I16 _ -> set_setting16_data
    | `I32 _ -> set_setting32_data in
  let s = to_hex_string x in
  let buf = Cstruct.create (String.length s) in
  setter s 0 buf;
  buf

let to_prefix ~request =
  let c, s, rw = Request.code request in
  let pfx = Cstruct.create sizeof_prefix in
  set_prefix_stx pfx stx;
  set_prefix_address (to_hex_string (`I8 address)) 0 pfx;
  set_prefix_category (to_hex_string (`I8 c)) 0 pfx;
  set_prefix_setting  (to_hex_string (`I8 s)) 0 pfx;
  set_prefix_rw pfx @@ rw_to_int rw;
  if (c >= 1 && c <= 3) then pfx else
    Cstruct.append pfx (serialize (`I16 0))

let to_suffix ~crc =
  let sfx = Cstruct.create sizeof_suffix in
  set_suffix_crc (to_hex_string (`I8 crc)) 0 sfx;
  set_suffix_etx sfx etx;
  sfx

let to_msg ~request ~body =
  let pfx = to_prefix ~request in
  let msg = Cstruct.append pfx body in
  let sfx = to_suffix ~crc:(calc_crc msg) in
  Cstruct.append msg sfx

let to_empty_msg ~request =
  to_msg ~request ~body:(Cstruct.create 0)

module Set = struct
  let bool r x = to_msg ~request:r ~body:(serialize (`I8 (if x then 1 else 0)))
  let int8 r x = to_msg ~request:r ~body:(serialize (`I8 x))
  let int16 r x = to_msg ~request:r ~body:(serialize (`I16 x))
  let int32 r x = to_msg ~request:r ~body:(serialize (`I32 x))
  let ipaddr r x = to_msg ~request:r ~body:(serialize (`I32 (Ipaddr.V4.to_int32 x)))
end
