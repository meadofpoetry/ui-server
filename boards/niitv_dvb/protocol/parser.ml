open Board_niitv_dvb_types
open Message

let ( % ) f g x = f (g x)

type error =
  | Bad_tag_start of int
  | Bad_length of int
  | Bad_msg_code of int
  | Bad_crc of (int * int)
  | Bad_tag_stop of int
  | Insufficient_payload of Cstruct.t

let error_to_string : error -> string = function
  | Insufficient_payload _ -> "insufficient payload"
  | Bad_length x -> Printf.sprintf "incorrect length: %d" x
  | Bad_tag_start x -> Printf.sprintf "incorrect start tag: 0x%x" x
  | Bad_msg_code x -> Printf.sprintf "incorrect msg code: 0x%x" x
  | Bad_tag_stop x -> Printf.sprintf "incorrect stop tag: 0x%x" x
  | Bad_crc (x, y) -> Printf.sprintf "incorrect crc: expected 0x%x, got 0x%x" x y

let max_uint16 = Unsigned.(UInt16.to_int UInt16.max_int)
let max_uint32 = Unsigned.(UInt32.to_int32 UInt32.max_int)

let parse_devinfo (buf : Cstruct.t) : (Device.info, Request.error) result =
  try
    let hw_cfg = get_rsp_devinfo_hw_config buf in
    let serial = get_rsp_devinfo_serial buf in
    let hw_ver = get_rsp_devinfo_hw_ver buf in
    let fpga_ver = get_rsp_devinfo_fpga_ver buf in
    let fw_ver = get_rsp_devinfo_soft_ver buf in
    let asi = (hw_cfg land 16) > 0 in
    let receivers =
      List.fold_left (fun acc x ->
          let x' = float_of_int x in
          if (hw_cfg land (int_of_float (2. ** x'))) > 0
          then x :: acc
          else acc) [] [3; 2; 1; 0]
    in
    Ok { serial; hw_ver; fpga_ver; fw_ver; asi; receivers }
  with Invalid_argument _ -> Error Invalid_length

let parse_source_id (buf : Cstruct.t) =
  try Ok (get_rsp_src_id_source_id buf)
  with Invalid_argument _ -> Error Request.Invalid_length

let parse_mode (buf : Cstruct.t) =
  try
    let ( >>= ) o f = match o with Some x -> f x | None -> None in
    let rsp =
      bool_of_int @@ get_mode_lock buf
      >>= fun lock -> bool_of_int @@ get_mode_hw_present buf
      >>= fun hw_present -> Device.standard_of_enum (get_mode_standard buf)
      >>= fun standard -> Device.bw_of_enum (get_mode_bw buf)
      >>= fun bw ->
      let freq = Int32.to_int @@ get_mode_freq buf in
      let plp = get_mode_plp buf in
      Some { Device. lock
           ; hw_present
           ; mode = { standard; channel = { bw; freq; plp }}
           } in
    match rsp with
    | None -> Error Request.Invalid_payload
    | Some x -> Ok x
  with Invalid_argument _ -> Error Invalid_length

let parse_measures (buf : Cstruct.t) =
  let int_to_opt x = if x = max_uint16 then None else Some x in
  let int32_to_opt x = if Int32.equal x max_uint32 then None else Some x in
  try
    match bool_of_int @@ get_rsp_measure_lock buf with
    | None -> Error Request.Invalid_payload
    | Some lock ->
      let power = match (int_to_opt % get_rsp_measure_power) buf with
        | None -> None
        | Some x -> Some (Float.neg @@ (float_of_int x) /. 10.) in
      let mer = match (int_to_opt % get_rsp_measure_mer) buf with
        | None -> None
        | Some x -> Some ((float_of_int x) /. 10.) in
      let ber = match (int32_to_opt % get_rsp_measure_ber) buf with
        | None -> None
        | Some x -> Some ((Int32.to_float x) /. (2. ** 24.)) in
      let freq = match (int32_to_opt % get_rsp_measure_freq) buf with
        | None -> None
        | Some x -> Some (Int32.to_int x) in
      let bitrate = match (int32_to_opt % get_rsp_measure_bitrate) buf with
        | None -> None
        | Some x -> Some (Int32.to_int x) in
      let data = { Measure. lock; power; mer; ber; freq; bitrate } in
      Ok { data; timestamp = Ptime_clock.now () }
  with Invalid_argument _ -> Error Invalid_length

let parse_params (buf : Cstruct.t) =
  try
    match bool_of_int @@ get_rsp_params_lock buf with
    | None -> Error Request.Invalid_payload
    | Some lock ->
      let data =
        { Params. lock
        ; fft = get_rsp_params_fft buf
        ; gi = get_rsp_params_gi  buf
        ; bw_ext = get_rsp_params_bw_ext buf > 0
        ; papr = get_rsp_params_papr buf
        ; l1_rep = get_rsp_params_l1_rep buf > 0
        ; l1_mod = get_rsp_params_l1_mod buf
        ; freq = Int32.to_int @@ get_rsp_params_freq buf
        ; l1_post_sz = get_rsp_params_l1_post_sz buf
        ; l1_post_info_sz = get_rsp_params_l1_post_info_sz buf
        ; tr_fmt = get_rsp_params_tr_fmt buf
        ; sys_id = get_rsp_params_sys_id buf
        ; net_id = get_rsp_params_net_id buf
        ; cell_id = get_rsp_params_cell_id buf
        ; t2_frames = get_rsp_params_t2_frames buf
        ; ofdm_syms = get_rsp_params_ofdm_syms buf
        ; pp = get_rsp_params_pp buf
        ; plp_num = get_rsp_params_plp_num buf
        ; tx_id_avail = get_rsp_params_tx_id_avail buf
        ; num_rf = get_rsp_params_num_rf buf
        ; cur_rf_id = get_rsp_params_cur_rf_id buf
        ; cur_plp_id = get_rsp_params_cur_plp_id buf
        ; plp_type = get_rsp_params_plp_type buf
        ; cr = get_rsp_params_cr buf
        ; plp_mod = get_rsp_params_plp_mod buf
        ; rotation = get_rsp_params_rotation buf > 0
        ; fec_sz = get_rsp_params_fec_size buf
        ; fec_block_num = get_rsp_params_fec_block_num buf
        ; in_band_flag = get_rsp_params_in_band_flag buf > 0
        } in
      Ok { data; timestamp = Ptime_clock.now () }
  with Invalid_argument _ -> Error Invalid_length

let parse_plp_list (buf : Cstruct.t) =
  try
    let opts, plps = Cstruct.split buf sizeof_rsp_plp_list in
    match bool_of_int @@ get_rsp_plp_list_lock opts with
    | None -> Error Request.Invalid_payload
    | Some lock ->
      let plps = match get_rsp_plp_list_plp_qty opts with
        | 0xFF -> []
        | n ->
          let iter =
            Cstruct.iter (fun _ -> Some 1)
              (fun buf -> Cstruct.get_uint8 buf 0)
              (fst @@ Cstruct.split plps n)
          in
          List.sort compare
          @@ Cstruct.fold (fun acc el -> el :: acc) iter []
      in
      let data = { Plp_list. lock; plps } in
      Ok { data; timestamp = Ptime_clock.now () }
  with Invalid_argument _ -> Error Invalid_length

let check_tag_start buf =
  try
    let tag = get_prefix_tag_start buf in
    if tag = tag_start then Ok buf
    else Error (Bad_tag_start tag)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_tag (buf : Cstruct.t) =
  try
    let tag = get_prefix_msg_code buf in
    match Request.tag_of_enum tag with
    | None -> Error (Bad_msg_code tag)
    | Some x -> Ok (x, buf)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_crc (tag, buf) =
  try
    let length = pred @@ get_prefix_length buf in
    let total = sizeof_prefix + length + sizeof_suffix in
    let msg, rest = Cstruct.split buf total in
    let pfx, msg' = Cstruct.split msg sizeof_prefix in
    let bdy, sfx = Cstruct.split msg' length in
    let stop = get_suffix_tag_stop sfx in
    if stop <> tag_stop
    then Error (Bad_tag_stop stop)
    else
      let crc = Serializer.calc_crc ~pfx bdy in
      let crc' = get_suffix_crc sfx in
      if crc <> crc'
      then Error (Bad_crc (crc, crc'))
      else Ok ({ Request. tag; data = bdy }, rest)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_msg msg =
  let ( >>= ) r f = match r with Ok x -> f x | Error e -> Error e in
  check_tag_start msg
  >>= check_tag
  >>= check_crc

let deserialize (src : Logs.src) buf =
  let rec aux responses b =
    if Cstruct.len b < (sizeof_prefix + 1 + sizeof_suffix)
    then responses, b
    else
      match check_msg b with
      | Ok (x, rest) -> aux (x :: responses) rest
      | Error e ->
        match e with
        | Insufficient_payload x -> responses, b
        | e ->
          Logs.err ~src (fun m -> m "parser error: %s" @@ error_to_string e);
          aux responses (Cstruct.shift b 1)
  in
  let responses, rest = aux [] buf in
  List.rev responses,
  if Cstruct.len rest > 0 then Some rest else None

let is_response (type a) (req : a Request.t)
    (msg : Request.msg) : (a, Request.error) result option =
  let ( >>= ) r f = match r with Ok x -> f x | Error e -> Error e in
  if not Request.(equal_tag msg.tag (to_tag req))
  then None
  else begin match req with
    | Reset -> Some (parse_devinfo msg.data)
    | Set_src_id id -> Some (parse_source_id msg.data)
    | Get_devinfo -> Some (parse_devinfo msg.data)
    | Set_mode (id, _) -> Some (parse_mode msg.data >>= fun x -> Ok (id, x))
    | Get_measure id -> Some (parse_measures msg.data >>= fun x -> Ok (id, x))
    | Get_params id -> Some (parse_params msg.data >>= fun x -> Ok (id, x))
    | Get_plp_list id -> Some (parse_plp_list msg.data >>= fun x -> Ok (id, x))
  end
