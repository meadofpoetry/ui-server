open Board_niitv_ts2ip_types
open Application_types

type error =
  | Invalid_start_tag of int
  | Invalid_msg_code of int
  | Insufficient_payload of Cstruct.t

let error_to_string = function
  | Invalid_start_tag x -> "invalid start tag: " ^ (string_of_int x)
  | Invalid_msg_code x -> "invalid code: " ^ (string_of_int x)
  | Insufficient_payload _ -> "insufficient payload"

let parse_devinfo (buf : Cstruct.t) =
  try
    let ver = Message.get_rsp_devinfo_ver buf in
    let typ = Message.get_rsp_devinfo_typ buf in
    let packers_num = Message.get_rsp_devinfo_packers_num buf in
    Ok { typ; ver; packers_num }
  with Invalid_argument _ -> Error Request.Invalid_length

let parse_udp_status (buf : Cstruct.t) =
  let rate = Message.get_udp_status_rate buf in
  let strm = Message.get_udp_status_stream buf in
  let (stream : Stream.Multi_TS_ID.t) =
    Stream.Multi_TS_ID.of_int32_pure Int32.(logand strm 0x3F_FF_FFl) in
  let flags = Int32.to_int @@ Int32.shift_right_logical rate 24 in
  let rdy = flags land 0x08 > 0 in
  let sync = flags land 0x10 > 0 in
  let overflow = flags land 0x20 > 0 in
  let enabled = Int32.((logand strm 0x80_00_00_00l) > 0l) in
  let bitrate =
    if not rdy then None
    else Some (Int32.(to_int @@ mul 8l (logand rate 0x07_FF_FF_FFl))) in
  { overflow
  ; enabled
  ; sync
  ; bitrate
  ; stream
  }

let parse_status_data (buf : Cstruct.t) =
  let iter =
    Cstruct.iter
      (fun _ -> Some Message.sizeof_udp_status)
      parse_udp_status
      buf
  in
  List.rev @@ Cstruct.fold (fun acc x -> x :: acc) iter []

let parse_status (buf : Cstruct.t) =
  let data = Message.get_status_data buf in
  let phy = Message.get_status_phy buf in
  let input = Message.get_status_input buf in
  let speed =
    if phy land 0x06 > 0 then Speed_1000
    else if phy land 0x0A > 0 then Speed_100
    else if phy land 0x12 > 0 then Speed_10
    else Speed_failure in
  let brd =
    { phy = phy land 1 > 0
    ; link = phy land 0x20 > 0
    ; speed
    ; spi_1 = (input land 0x01) > 0
    ; spi_2 = (input land 0x02) > 0
    ; spi_3 = (input land 0x04) > 0
    ; asi_1 = (input land 0x08) > 0
    ; asi_2 = (input land 0x10) > 0
    ; udp = parse_status_data data
    } in
  brd

let check_prefix (buf : Cstruct.t) =
  try
    let tag = Message.get_prefix_tag_start buf in
    if Message.tag_start = tag then Ok buf
    else Error (Invalid_start_tag tag)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_msg_code (buf : Cstruct.t) =
  try
    let pfx, rest = Cstruct.split buf Message.sizeof_prefix in
    let code = Message.get_prefix_msg_code pfx in
    match Request.rsp_tag_of_enum code with
    | None | Some `Devinfo_req | Some `Mode | Some `MAC ->
      Error (Invalid_msg_code code)
    | Some ((`Devinfo_rsp | `Status) as tag) ->
      let length = Request.tag_to_data_size tag in
      let data, rest = Cstruct.split rest length in
      Ok ({ Request. tag; data }, rest)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let get_msg (buf : Cstruct.t) =
  let ( >>= ) r f = match r with Ok x -> f x | Error e -> Error e in
  check_prefix buf
  >>= check_msg_code

let deserialize (src : Logs.src) (buf : Cstruct.t) =
  let rec aux responses (buf : Cstruct.t) =
    if Cstruct.len buf >= Message.sizeof_prefix
    then match get_msg buf with
      | Ok (x, rest) ->
        (match x.tag with
         | `Status ->
           print_endline @@ show_status @@ parse_status x.data;
         | _ -> ());
        aux (x :: responses) rest
      | Error e ->
        match e with
        | Insufficient_payload x -> (responses, x)
        | e ->
          Logs.warn ~src (fun m -> m "parser error: %s" @@ error_to_string e);
          aux responses (Cstruct.shift buf 1)
    else (responses, buf)
  in
  let responses, rest = aux [] buf in
  List.rev responses,
  if Cstruct.len rest > 0 then Some rest else None

let is_response (type a) (req : a Request.t) msg : (a, Request.error) result option =
  match req with
  | Get_devinfo -> None
  | Set_mode_main _ -> None
  | Set_mode_aux_1 _ -> None
  | Set_mode_aux_2 _ -> None
  | Set_mac _ -> None
