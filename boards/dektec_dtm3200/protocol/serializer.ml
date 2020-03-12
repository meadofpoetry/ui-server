open Request

let size_of_data : data -> int = function
  | `B _ | `I8 _ -> Message.sizeof_setting8
  | `I16 _ -> Message.sizeof_setting16
  | `I32 _ -> Message.sizeof_setting32

let calc_crc ~pfx ~bdy =
  let iter =
    Cstruct.iter
      (fun _ -> Some 1)
      (fun buf -> Cstruct.get_uint8 buf 0)
      (snd (Cstruct.split (Cstruct.append pfx bdy) 1))
  in
  let sum = Cstruct.fold ( + ) iter 0 in
  (lnot sum + 1) land 0xFF

let to_hex_string x =
  let size = size_of_data x in
  let s =
    match x with
    | `B x -> Printf.sprintf "%X" (if x then 1 else 0)
    | `I8 x | `I16 x -> Printf.sprintf "%X" x
    | `I32 x -> Printf.sprintf "%lX" x
  in
  match String.length s with
  | len when len < size -> String.make (size - len) '0' ^ s
  | len when len > size -> String.sub s (len - size) size
  | _ -> s

let serialize_data (x : data) =
  let setter : data -> string -> int -> Cstruct.t -> unit = function
    | `B _ | `I8 _ -> Message.set_setting8_data
    | `I16 _ -> Message.set_setting16_data
    | `I32 _ -> Message.set_setting32_data
  in
  let buf = Cstruct.create (size_of_data x) in
  setter x (to_hex_string x) 0 buf;
  buf

let serialize ~address request =
  Message.(
    let { category; setting; rw; data } = Request.to_cmd request in
    let pfx = Cstruct.create sizeof_prefix in
    let bdy =
      match data with None -> Cstruct.create 0 | Some x -> serialize_data x
    in
    let sfx = Cstruct.create sizeof_suffix in
    set_prefix_stx pfx stx;
    set_prefix_address (to_hex_string (`I8 address)) 0 pfx;
    set_prefix_category (to_hex_string (`I8 (category_to_enum category))) 0 pfx;
    set_prefix_setting (to_hex_string (`I8 setting)) 0 pfx;
    set_prefix_rw pfx (access_to_int rw);
    let pfx =
      match category with
      | `Device | `Configuration | `Network -> pfx
      | `IP_receive | `ASI_output ->
          Cstruct.append pfx (serialize_data (`I16 0))
    in
    set_suffix_crc (to_hex_string (`I8 (calc_crc ~pfx ~bdy))) 0 sfx;
    set_suffix_etx sfx etx;
    Cstruct.concat [ pfx; bdy; sfx ])
