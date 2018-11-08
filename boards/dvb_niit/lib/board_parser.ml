open Containers
open Board_msg_formats

let tag_start = 0x55AA
let tag_stop  = 0xFE

let max_uint16 = Unsigned.(UInt16.to_int UInt16.max_int)
let max_uint32 = Unsigned.(UInt32.to_int32 UInt32.max_int)

open Board_types

exception Parse_error

(* Misc *)

module type Src = sig
  val source_id : int
end

module Make(Logs : Logs.LOG) = struct

  type _ request =
    | Get_devinfo : Device.devinfo request
    | Reset : unit request
    | Set_src_id : int -> int request
    | Set_mode : (int * Device.mode) -> (int * Device.mode_rsp) request

  type event =
    | Measures of (int * Measure.t)
    | Params of (int * Params.t)
    | Plp_list of (int * Plp_list.t) [@@deriving show]

  type _ event_request =
    | Get_measure : int -> event event_request
    | Get_params : int -> event event_request
    | Get_plp_list : int -> event event_request

  (* Helper functions *)

  let calc_crc (msg : Cstruct.t) =
    let _, body = Cstruct.split msg (sizeof_prefix - 1) in
    let iter = Cstruct.iter (fun _ -> Some 1)
                 (fun buf -> Cstruct.get_uint8 buf 0)
                 body in
    Cstruct.fold (lxor) iter 0

  (* Message constructors *)

  let to_prefix ~length ~msg_code =
    let prefix = Cstruct.create sizeof_prefix in
    set_prefix_tag_start prefix tag_start;
    set_prefix_length prefix length;
    set_prefix_msg_code prefix msg_code;
    prefix

  let to_suffix ~crc =
    let suffix = Cstruct.create sizeof_suffix in
    set_suffix_crc suffix crc;
    set_suffix_tag_stop suffix tag_stop;
    suffix

  let to_msg ~msg_code ~body =
    let length = Cstruct.len body + 1 in
    let prefix = to_prefix ~length ~msg_code in
    let msg = Cstruct.append prefix body in
    let suffix = to_suffix ~crc:(calc_crc msg) in
    Cstruct.append msg suffix

  let to_empty_msg ~msg_code =
    let body = Cstruct.create 1 in
    to_msg ~msg_code ~body

  (* Requests/responses *)

  (* Src id *)

  let to_src_id_req (source_id : int) =
    let body = Cstruct.create sizeof_cmd_src_id in
    set_cmd_src_id_source_id body source_id;
    to_msg ~msg_code:0xD0 ~body

  let parse_src_id_rsp_exn msg =
    try
      get_rsp_src_id_source_id msg
    with _ -> raise Parse_error

  (* Devinfo *)

  let to_devinfo_req reset =
    let body = Cstruct.create sizeof_cmd_devinfo in
    set_cmd_devinfo_reset body (if reset then 0xFF else 0);
    to_msg ~msg_code:0x10 ~body

  let parse_devinfo_rsp_exn msg : Device.devinfo =
    try
      let hw_cfg = get_rsp_devinfo_hw_config msg in
      { serial = get_rsp_devinfo_serial msg
      ; hw_ver = get_rsp_devinfo_hw_ver msg
      ; fpga_ver = get_rsp_devinfo_fpga_ver msg
      ; soft_ver = get_rsp_devinfo_soft_ver msg
      ; asi = if (hw_cfg land 16) > 0 then true else false
      ; receivers =
          List.fold_left (fun acc x ->
              let x' = float_of_int x in
              if (hw_cfg land (int_of_float (2. ** x'))) > 0
              then x :: acc
              else acc) [] (List.rev @@ List.range 0 3)
      }
    with _ -> raise Parse_error

  (* Mode *)

  let to_mode_req id (mode : Device.mode) =
    let open Device in
    let body = Cstruct.create sizeof_mode in
    set_mode_standard body (standard_to_enum mode.standard);
    set_mode_bw body (bw_to_enum mode.channel.bw);
    set_mode_freq body @@ Int32.of_int mode.channel.freq;
    set_mode_plp body mode.channel.plp;
    to_msg ~msg_code:(0x20 lor id) ~body

  let parse_mode_rsp_exn id msg : int * Device.mode_rsp =
    try
      let open Option in
      let open Device in
      let lock =
        int_to_bool8 (get_mode_lock msg)
        |> get_exn
        |> bool_of_bool8 in
      let hw_present =
        int_to_bool8 (get_mode_hw_present msg)
        |> get_exn
        |> bool_of_bool8 in
      let standard = get_exn @@ standard_of_enum (get_mode_standard msg) in
      let bw = get_exn @@ bw_of_enum (get_mode_bw msg) in
      let freq = Int32.to_int @@ get_mode_freq msg in
      let plp = get_mode_plp msg in
      id, { lock
          ; hw_present
          ; mode = { standard; channel = { bw; freq; plp }}
          }
    with _ -> raise Parse_error

  (* Measure *)

  let to_measure_req id =
    to_empty_msg ~msg_code:(0x30 lor id)

  let parse_measures_rsp_exn id msg : int * Measure.t =
    let int_to_opt x = if Int.equal x max_uint16 then None else Some x in
    let int32_to_opt x = if Int32.equal x max_uint32 then None else Some x in
    try
      let lock =
        int_to_bool8 (get_rsp_measure_lock msg)
        |> Option.get_exn |> bool_of_bool8 in
      let power =
        Fun.(int_to_opt % get_rsp_measure_power) msg
        |> Option.map (fun x -> -.((float_of_int x) /. 10.)) in
      let mer =
        Fun.(int_to_opt % get_rsp_measure_mer) msg
        |> Option.map (fun x -> (float_of_int x) /. 10.) in
      let ber =
        Fun.(int32_to_opt % get_rsp_measure_ber) msg
        |> Option.map (fun x -> (Int32.to_float x) /. (2.**24.)) in
      let freq =
        Fun.(int32_to_opt % get_rsp_measure_freq) msg
        |> Option.map Int32.to_int in
      let bitrate =
        Fun.(int32_to_opt % get_rsp_measure_bitrate) msg
        |> Option.map Int32.to_int in
      let (data : Measure.t) =
        { lock; power; mer; ber; freq; bitrate } in
      id, data
    with _ -> raise Parse_error

  (* Params *)

  let to_params_req id =
    to_empty_msg ~msg_code:(0x40 lor id)

  let parse_params_rsp_exn id msg : int * Params.t =
    let bool_of_int x = x <> 0 in
    try
      let lock =
        int_to_bool8 (get_rsp_params_lock msg)
        |> Option.get_exn
        |> bool_of_bool8 in
      let (data : Params.t) =
        { lock
        ; fft = get_rsp_params_fft msg
        ; gi = get_rsp_params_gi  msg
        ; bw_ext = get_rsp_params_bw_ext msg |> bool_of_int
        ; papr = get_rsp_params_papr msg
        ; l1_rep = get_rsp_params_l1_rep msg |> bool_of_int
        ; l1_mod = get_rsp_params_l1_mod msg
        ; freq = get_rsp_params_freq msg |> Int32.to_int
        ; l1_post_sz = get_rsp_params_l1_post_sz msg
        ; l1_post_info_sz = get_rsp_params_l1_post_info_sz msg
        ; tr_fmt = get_rsp_params_tr_fmt msg
        ; sys_id = get_rsp_params_sys_id msg
        ; net_id = get_rsp_params_net_id msg
        ; cell_id = get_rsp_params_cell_id msg
        ; t2_frames = get_rsp_params_t2_frames msg
        ; ofdm_syms = get_rsp_params_ofdm_syms msg
        ; pp = get_rsp_params_pp msg
        ; plp_num = get_rsp_params_plp_num msg
        ; tx_id_avail = get_rsp_params_tx_id_avail msg
        ; num_rf = get_rsp_params_num_rf msg
        ; cur_rf_id = get_rsp_params_cur_rf_id msg
        ; cur_plp_id = get_rsp_params_cur_plp_id msg
        ; plp_type = get_rsp_params_plp_type msg
        ; cr = get_rsp_params_cr msg
        ; plp_mod = get_rsp_params_plp_mod msg
        ; rotation = get_rsp_params_rotation msg |> bool_of_int
        ; fec_sz = get_rsp_params_fec_size msg
        ; fec_block_num = get_rsp_params_fec_block_num msg
        ; in_band_flag = get_rsp_params_in_band_flag msg |> bool_of_int
        }
      in
      id, data
    with _ -> raise Parse_error

  (* PLP list *)

  let to_plp_list_req id =
    to_empty_msg ~msg_code:(0x50 lor id)

  let parse_plp_list_rsp_exn id msg : int * Plp_list.t =
    try
      let plp_num =
        Cstruct.get_uint8 msg 1
        |> (fun x -> if x = 0xFF then None else Some x) in
      (* let lock =
       *   int_to_bool8 (Cstruct.get_uint8 msg 0)
       *   |> Option.get_exn
       *   |> bool_of_bool8 in *)
      let plps = match plp_num with
        | None -> []
        | Some _ ->
           let iter =
             Cstruct.iter (fun _ -> Some 1)
               (fun buf -> Cstruct.get_uint8 buf 0)
               (Cstruct.shift msg 2) in
           Cstruct.fold (fun acc el -> el :: acc) iter []
           |> List.sort Int.compare in
      id, plps
    with _ -> raise Parse_error

  type parsed =
    { code : int
    ; body : Cstruct.t
    ; res : Cstruct.t
    }

  type err =
    | Bad_tag_start of int
    | Bad_length of int
    | Bad_msg_code of int
    | Bad_crc of (int * int)
    | Bad_tag_stop of int
    | Insufficient_payload of Cstruct.t
    | Unknown_err of string

  let err_to_string = function
    | Bad_tag_start x ->
       Printf.sprintf "incorrect start tag: 0x%x" x
    | Bad_length x ->
       Printf.sprintf "incorrect length: %d" x
    | Bad_msg_code x ->
       Printf.sprintf "incorrect msg code: 0x%x" x
    | Bad_crc (x,y) ->
       Printf.sprintf "incorrect crc: expected 0x%x, got 0x%x" x y
    | Bad_tag_stop x ->
       Printf.sprintf "incorrect stop tag: 0x%x" x
    | Insufficient_payload _ ->
       "insufficient payload"
    | Unknown_err s -> s

  let split_code (code:int) =
    code land 0x0F, code lsr 4

  let check_tag_start buf =
    let tag = get_prefix_tag_start buf in
    if tag <> tag_start
    then Error (Bad_tag_start tag) else Ok buf

  let check_length buf =
    let length = get_prefix_length buf in
    if (length < 2) || (length > 41)
    then Error (Bad_length length) else Ok buf

  let check_msg_code buf =
    match get_prefix_msg_code buf with
    | 0xEE as x -> Ok (x, buf) (* ack *)
    | 0xD0 as x -> Ok (x, buf) (* src id *)
    | 0x10 as x -> Ok (x, buf) (* devinfo *)
    | code ->
       (* other *)
       begin match split_code code with
       | id, c when (id >= 0 && id < 4) && (c > 1 && c < 7) ->
          Ok (code, buf)
       | _ -> Error (Bad_msg_code code)
       end

  let check_msg_crc (code, buf) =
    let payload_len = (get_prefix_length buf) - 1 in
    let total_len = payload_len + sizeof_prefix + sizeof_suffix in
    let msg, res = Cstruct.split buf total_len in
    let pfx, msg' = Cstruct.split msg sizeof_prefix in
    let body, sfx = Cstruct.split msg' payload_len in
    let tag = get_suffix_tag_stop sfx in
    if tag <> tag_stop then Error (Bad_tag_stop tag) else
      let crc = (calc_crc (Cstruct.append pfx body)) in
      let crc' = get_suffix_crc sfx in
      if crc <> crc' then Error (Bad_crc (crc, crc'))
      else Ok { code; body; res }

  let check_msg msg =
    try
      Result.(
      check_tag_start msg
      >>= check_length
      >>= check_msg_code
      >>= check_msg_crc)
    with
    | Invalid_argument _ -> Error (Insufficient_payload msg)
    | e -> Error (Unknown_err (Printexc.to_string e))

  let parse_msg = fun { code; body; _ } ->
    match code with
    | 0xEE ->
       Logs.debug (fun m -> m "deserializer - got ack");
       `R `Ack
    | 0x10 ->
       Logs.debug (fun m -> m "deserializer - got devinfo");
       `R (`Devinfo body)
    | 0xD0 ->
       Logs.debug (fun m -> m "deserializer - got source id");
       `R (`Src_id body)
    | code ->
       let id, code' = split_code code in
       begin match code' with
       | 2 ->
          Logs.debug (fun m -> m "deserializer - got settings (%d)" id);
          `R (`Settings (id, body))
       | 3 ->
          Logs.debug (fun m -> m "deserializer - got measure (%d)" id);
          `E (`Measure (id, body))
       | 4 ->
          Logs.debug (fun m -> m "deserializer - got params (%d)" id);
          `E (`Params (id, body))
       | 5 ->
          Logs.debug (fun m -> m "deserializer - got plp list (%d)" id);
          `E (`Plps (id, body))
       | 6 ->
          Logs.debug (fun m -> m "deserializer - got plp setting (%d)" id);
          `R (`Plp_setting (id, body))
       | _ -> Logs.warn (fun m ->
                  m "deserializer - unknown message code 0x%x" code); `N
       end

  let deserialize buf =
    let rec f events responses b =
      if Cstruct.len b <= (sizeof_prefix + 1 + sizeof_suffix)
      then List.rev events, List.rev responses, b
      else
        match check_msg b with
        | Ok x ->
           parse_msg x
           |> (function
               | `E e -> f (e :: events) responses x.res
               | `R r -> f events (r :: responses) x.res
               | `N -> f events responses x.res)
        | Error e ->
           begin match e with
           | Insufficient_payload x ->
              List.rev events, List.rev responses, x
           | e ->
              Logs.warn (fun m ->
                  m "deserializer - parser error: %s" @@ err_to_string e);
              f events responses (Cstruct.shift b 1)
           end in
    let events, responses, res = f [] [] buf in
    events, responses, if Cstruct.len res > 0 then Some res else None

  let try_parse f x =
    try Some (f x) with Parse_error -> None

  let parse_src_id_rsp = function
    | `Src_id buf -> try_parse parse_src_id_rsp_exn buf
    | _ -> None

  let parse_devinfo_rsp = function
    | `Devinfo buf -> try_parse parse_devinfo_rsp_exn buf
    | _ -> None

  let parse_reset_rsp = function
    | `Ack -> Some ()
    | _ -> None

  let parse_mode_rsp id = function
    | `Settings (idx, buf) when idx = id ->
       try_parse (fun b -> parse_mode_rsp_exn id b) buf
    | _ -> None

  let parse_measures_rsp id = function
    | `Measure (idx, buf) when idx = id ->
       try_parse (fun b -> Measures (parse_measures_rsp_exn id b)) buf
    | _ -> None

  let parse_params_rsp id = function
    | `Params (idx, buf) when idx = id ->
       try_parse (fun b -> Params (parse_params_rsp_exn id b)) buf
    | _ -> None

  let parse_plp_list_rsp id = function
    | `Plps (idx, buf) when idx = id ->
       try_parse (fun b -> Plp_list (parse_plp_list_rsp_exn id b)) buf
    | _ -> None

  let is_response (type a) (req : a request) msg : a option =
    match req with
    | Reset -> parse_reset_rsp msg
    | Set_src_id _ -> parse_src_id_rsp msg
    | Get_devinfo -> parse_devinfo_rsp msg
    | Set_mode (id, _) -> parse_mode_rsp id msg

  let is_event (type a) (req : a event_request) msg : a option =
    match req with
    | Get_measure id -> parse_measures_rsp id msg
    | Get_params id -> parse_params_rsp   id msg
    | Get_plp_list id -> parse_plp_list_rsp id msg

end
