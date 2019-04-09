open Board_dektec_dtm3200_types
open Request
open Message

let ( % ) f g x = f (g x)

type err =
  | Bad_stx of int
  | Bad_etx of int
  | Bad_length of int
  | Bad_address of int
  | Bad_category of int
  | Bad_setting of category * int
  | Bad_rw of int
  | Bad_crc of int * int
  | Bad_value of Cstruct.t
  | Insufficient_payload of Cstruct.t
  | Unknown of string

let err_to_string error =
  Printf.(
    match error with
    | Bad_stx x -> sprintf "incorrect STX: %d" x
    | Bad_etx x -> sprintf "incorrect ETX: %d" x
    | Bad_length x -> sprintf "incorrect length: %d" x
    | Bad_address x -> sprintf "incorrect address: %d" x
    | Bad_category x -> sprintf "incorrect category: %d" x
    | Bad_setting (x, y) ->
       sprintf "incorrect setting: %d, in category %s(0x%02X)"
         y (category_to_string x) (category_to_enum x)
    | Bad_rw x -> sprintf "incorrect rw: %d" x
    | Bad_crc (x, y) -> sprintf "incorrect crc, expected %d, got %d" x y
    | Bad_value x -> sprintf "incorrect value: %s" (Cstruct.to_string x)
    | Insufficient_payload _ -> "insufficient payload"
    | Unknown s -> s)

let check_stx buf =
  try match get_prefix_stx buf with
      | x when x = stx -> Ok buf
      | x -> Error (Bad_stx x)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_address ~address buf =
  try
    let addr = get_prefix_address buf in
    match Ascii.Int.get addr with
    | Some x when x = address -> Ok buf
    | Some x -> Error (Bad_address x)
    | None -> Error (Bad_value addr)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_category buf =
  try
    let cat = get_prefix_category buf in
    match Ascii.Int.get cat with
    | None -> Error (Bad_value cat)
    | Some x ->
       match category_of_enum x with
       | None -> Error (Bad_category x)
       | Some c -> Ok (c, buf)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_setting (cat, buf) =
  try
    let set = get_prefix_setting buf in
    match Ascii.Int.get set with
    | None -> Error (Bad_value set)
    | Some x ->
       match response_data_size (cat, x) with
       | None -> Error (Bad_setting (cat, x))
       | Some length -> Ok (cat, x, length, buf)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_rw (cat, set, length, buf) =
  try
    let rw = get_prefix_rw buf in
    match access_of_int rw with
    | Some x -> Ok (cat, set, length, x, buf)
    | None -> Error (Bad_rw rw)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_rest (category, setting, length, rw, buf) =
  let length = match rw with E -> 0 | _ -> length in
  let pfx_len = match category with
    | `Device | `Configuration | `Network -> sizeof_prefix
    | `IP_receive | `ASI_output -> sizeof_prefix + sizeof_setting16 in
  if Cstruct.len buf < (pfx_len + length + sizeof_suffix)
  then Error (Insufficient_payload buf)
  else (
    let pfx, msg' = Cstruct.split buf pfx_len in
    let bdy, rst' = Cstruct.split msg' length in
    let sfx, rest = Cstruct.split rst' sizeof_suffix in
    (* No need to catch exception here because we've already checked length. *)
    let crc' = Ascii.Int.get_exn @@ get_suffix_crc sfx in
    let etx' = get_suffix_etx sfx in
    let crc = Serializer.calc_crc ~pfx ~bdy in
    if crc' <> crc then Error (Bad_crc (crc, crc'))
    else if etx' <> etx then Error (Bad_etx etx')
    else Ok ({ category; setting; rw; data = bdy }, rest))

let get_msg ~address buf =
  let ( >>= ) r f = match r with Error e -> Error e | Ok x -> f x in
  check_stx buf
  >>= check_address ~address
  >>= check_category
  >>= check_setting
  >>= check_rw
  >>= check_rest

let deserialize ~address src buf =
  let rec aux responses b =
    if Cstruct.len b > (sizeof_prefix + sizeof_suffix)
    then
      match get_msg ~address b with
      | Ok (x, rest) -> aux (x :: responses) rest
      | Error e ->
         begin match e with
         | Insufficient_payload x -> List.rev responses, x
         | e -> Logs.warn ~src (fun m -> m "parser error: %s" @@ err_to_string e);
                aux responses (Cstruct.shift b 1)
         end
    else List.rev responses, b in
  let responses, rest = aux [] buf in
  List.rev responses,
  if Cstruct.len rest > 0 then Some rest else None

let is_response (type a) (req : a Request.t) (m : Cstruct.t cmd) : a rsp option =
  match req with
  | Device req -> Device.of_cmd req m
  | Configuration req -> Configuration.of_cmd req m
  | Network req -> Network.of_cmd req m
  | IP_receive req -> Ip_receive.of_cmd req m
  | ASI_output req -> Asi_output.of_cmd req m
