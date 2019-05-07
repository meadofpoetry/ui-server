open Application_types
open Board_niitv_tsan_types
open Aux_types

module List = Boards.Util.List

type error =
  | Bad_prefix of int
  | Bad_length of int
  | Bad_msg_code of int
  | Bad_crc of int * int * int
  | Insufficient_payload of Cstruct.t

let error_to_string = function
  | Bad_prefix x -> "invalid prefix: " ^ (string_of_int x)
  | Bad_length x -> "invalid length: " ^ (string_of_int x)
  | Bad_msg_code x -> "invalid code: " ^ (string_of_int x)
  | Insufficient_payload _ -> "insufficient payload"
  | Bad_crc (code, x, y) ->
    Printf.sprintf "invalid CRC in message with code = 0x%x,\
                    expected %d, got %d" code x y

type part_id =
  { code : int
  ; request_id : int
  ; client_id : int
  } [@@deriving eq]

type part =
  { first : bool
  ; param : int32
  ; data : Cstruct.t
  }

let int8_to_bool_list (x : int) =
  List.map (fun i -> (x land i) > 0)
  @@ [0; 2; 4; 8; 16; 32; 64; 128]

let parse_devinfo (msg : Cstruct.t) =
  try
    Ok { typ = Message.get_board_info_board_type msg
       ; ver = Message.get_board_info_board_version msg
       }
  with _ -> Error Request.Invalid_payload

let parse_section
    ~(table_id : int)
    ~(section : int option)
    (msg : Cstruct.t) =
  try
    let hdr, bdy = Cstruct.split msg Message.sizeof_section in
    let length = Message.get_section_length hdr in
    let result = Message.get_section_result hdr in
    let timestamp = Ptime_clock.now () in
    if length > 0 && result = 0
    then
      let sid, data  = Cstruct.split bdy 4 in
      let stream_id = Cstruct.LE.get_uint32 sid 0 in
      let raw = Cstruct.to_string data in
      let section_id = match section with
        | None -> 0
        | Some x -> x in
      let data =
        { SI_PSI_section.Dump.
          section = B64.encode raw
        ; stream_id = Stream.Multi_TS_ID.of_int32_pure stream_id
        ; table_id
        ; section_id
        ; content = Si_psi_parser.parse raw
        } in
      Ok { timestamp; data }
    else
      let error = match result with
        | 0 | 3 -> Request.Custom "Section is empty"
        | 1 -> Custom "Table not found"
        | 2 -> Custom "Section not found"
        | 4 -> Custom "Stream_not_found"
        | x -> Custom (Printf.sprintf "Unknown error code %d" x) in
      Error error
  with Invalid_argument _ -> Error Request.Invalid_payload

let parse_t2mi_sequence (msg : Cstruct.t) =
  let iter =
    Cstruct.iter
      (fun _ -> Some Message.sizeof_t2mi_frame_seq_item)
      (fun x -> x) msg in
  let timestamp = Ptime_clock.now () in
  try
    let data =
      List.rev
      @@ Cstruct.fold (fun (acc : T2mi_sequence.item list) el ->
          let typ = Message.get_t2mi_frame_seq_item_typ el in
          let sframe_stream = Message.get_t2mi_frame_seq_item_sframe_stream el in
          let super_frame = (sframe_stream land 0xF0) lsr 4 in
          let stream_id = sframe_stream land 0x07 in
          let frame = Message.get_t2mi_frame_seq_item_frame el in
          let count = Int32.to_int @@ Message.get_t2mi_frame_seq_item_count el in
          let plp = Message.get_t2mi_frame_seq_item_plp el in
          let l1_param_1 = Message.get_t2mi_frame_seq_item_dyn1_frame el in
          let l1_param_2 = Message.get_t2mi_frame_seq_item_dyn2_frame el in
          let ts_packet = Int32.to_int @@ Message.get_t2mi_frame_seq_item_time el in
          { typ
          ; super_frame
          ; stream_id
          ; frame
          ; count
          ; plp
          ; l1_param_1
          ; l1_param_2
          ; ts_packet
          } :: acc)
        iter []
    in
    Ok { timestamp; data }
  with Invalid_argument _ -> Error Request.Invalid_payload

(* let parse_status (msg : Cstruct.t) =
 *   let time = Ptime_clock.now () in
 *   let iter =
 *     Cstruct.iter
 *       (fun _ -> Some 1)
 *       (fun buf -> Cstruct.get_uint8 buf 0) in
 *   let flags = Message.get_status_flags msg in
 *   let has_sync = not (flags land 0x04 > 0) in
 *   let ts_num = Message.get_status_ts_num msg in
 *   let flags2 = Message.get_status_flags_2 msg in
 *   let input, t2mi_mode =
 *     to_mode_exn (Message.get_status_mode msg)
 *       (Message.get_status_t2mi_pid msg)
 *       (Stream.Multi_TS_ID.of_int32_pure
 *        @@ Message.get_status_t2mi_stream_id msg) in
 *   let load = (float_of_int ((Message.get_status_load msg) * 100)) /. 255. in
 *   let reset = flags2 land 0x02 <> 0 in
 *   let bitrate = Int32.to_int @@ Message.get_status_bitrate msg in
 *   let ts_num = if has_sync then ts_num else 0 in
 *   let services_num = if has_sync then Message.get_status_services_num msg else 0 in
 *   let packet_sz =
 *     if (flags land 0x08) <> 0 then Ts192
 *     else if (flags land 0x10) <> 0 then Ts204
 *     else Ts188 in
 *   let (basic : status) =
 *     { time
 *     ; load
 *     ; reset
 *     ; ts_num
 *     ; services_num
 *     ; bitrate
 *     ; packet_sz
 *     ; has_sync
 *     ; has_stream = flags land 0x80 = 0
 *     } in
 *   let versions =
 *     { streams_ver = Message.get_status_streams_ver msg
 *     ; ts_ver_com = Message.get_status_ts_ver_com msg
 *     ; ts_ver_lst = Cstruct.fold (fun acc el -> el :: acc)
 *           (iter @@ Message.get_status_ts_ver_lst msg) []
 *                    |> List.rev
 *                    |> List.take ts_num
 *     ; t2mi_ver_lst = Message.get_status_t2mi_ver_lst msg
 *                      |> (fun v -> List.map (fun x ->
 *                          Int32.shift_right v (4 * x)
 *                          |> Int32.logand 0xfl
 *                          |> Int32.to_int)
 *                          (List.range 0 7))
 *     } in
 *   let raw =
 *     { basic
 *     ; input
 *     ; t2mi_mode
 *     ; jitter_mode =
 *         (let pid = Message.get_status_jitter_pid msg in
 *          let id = Message.get_status_jitter_stream_id msg in
 *          if not (pid = 0x1fff)
 *          then Some { stream = Stream.Multi_TS_ID.of_int32_pure id; pid }
 *          else None)
 *     ; errors = flags land 0x20 <> 0
 *     ; t2mi_sync =
 *         int_to_bool_list (Message.get_status_t2mi_sync msg)
 *         |> (fun l -> List.foldi (fun acc i x ->
 *             if x then i :: acc else acc) [] l)
 *     ; version = Message.get_status_version msg
 *     ; versions
 *     ; streams = [] (\* Filled in later *\)
 *     } in
 *   raw *)

module Deverr_parser = struct

  let param_codes = [18; 32]

  let traverse time (code, acc) item =
    let is_param = List.mem ~eq:(=) code param_codes in
    let acc =
      if not is_param
      then
        let item =
          { Board_error.
            time
          ; source = Hardware
          ; code
          ; count = Int32.to_int item
          ; param = None
          } in
        (item :: acc)
      else match acc with
        | hd :: tl -> { hd with param = Some (Int32.to_int item) } :: tl
        | _ -> acc in
    (succ code, acc)

  let parse (msg : Cstruct.t) =
    let iter =
      Cstruct.iter (fun _ -> Some 4)
        (fun buf -> Cstruct.LE.get_uint32 buf 0)
        (Message.get_board_errors_errors msg) in
    let time = Ptime_clock.now () in
    try Ok (List.rev @@ snd @@ Cstruct.fold (traverse time) iter (0, []))
    with Invalid_argument _ -> Error Request.Invalid_payload

end

module Bitrate_parser = struct

  let of_pids_bitrate total_pids br_per_pkt (buf : Cstruct.t) =
    let msg, rest = Cstruct.split buf (Message.sizeof_pid_bitrate * total_pids) in
    let iter =
      Cstruct.iter
        (fun _ -> Some Message.sizeof_pid_bitrate)
        (fun x -> x) msg in
    let pids =
      Cstruct.fold (fun acc el ->
          let packets = Message.get_pid_bitrate_packets el in
          let pid = Message.get_pid_bitrate_pid el land 0x1FFF in
          let br = int_of_float @@ br_per_pkt *. (Int32.to_float packets) in
          (pid, br) :: acc)
        iter []
    in
    List.rev pids, rest

  let of_tables_bitrate (total_tbls : int)
      (br_per_pkt : float)
      (buf : Cstruct.t) =
    let msg, _ = Cstruct.split buf (Message.sizeof_table_bitrate * total_tbls) in
    let iter =
      Cstruct.iter
        (fun _ -> Some Message.sizeof_table_bitrate)
        (fun x -> x) msg in
    let tables =
      Cstruct.fold (fun acc el ->
          let packets = Message.get_table_bitrate_packets el in
          let flags = Message.get_table_bitrate_flags el in
          let id_ext_1 = Message.get_table_bitrate_id_ext_1 el in
          let id_ext_2 = Message.get_table_bitrate_id_ext_2 el in
          let rate = int_of_float @@ br_per_pkt *. (Int32.to_float packets) in
          { Bitrate.
            table_id = Message.get_table_bitrate_table_id el
          ; table_id_ext = Message.get_table_bitrate_table_id_ext el
          ; id_ext_1
          ; id_ext_2
          ; fully_analyzed = flags land 2 > 0
          ; section_syntax = flags land 1 > 0
          ; bitrate = rate } :: acc)
        iter []
    in
    List.rev tables

  let of_stream_bitrate (buf : Cstruct.t) =
    let length = (Int32.to_int @@ Message.get_stream_bitrate_length buf) in
    let msg, rest = Cstruct.split buf (length + 8) in
    let hdr, bdy = Cstruct.split msg Message.sizeof_stream_bitrate in
    let total = Int32.to_int @@ Message.get_stream_bitrate_ts_bitrate hdr in
    let total_pkts = Message.get_stream_bitrate_total_packets hdr in
    let br_per_pkt = (float_of_int total) /. (Int32.to_float total_pkts)  in
    let total_pids = Message.get_stream_bitrate_total_pids hdr in
    let total_tbls = Message.get_stream_bitrate_total_tables hdr in
    let pids, tbls = of_pids_bitrate total_pids br_per_pkt bdy in
    let tables = of_tables_bitrate total_tbls br_per_pkt tbls in
    let stream = Message.get_stream_bitrate_stream_id hdr in
    let data = { Bitrate. total; pids; tables } in
    let rsp = Stream.Multi_TS_ID.of_int32_pure stream, data in
    rsp, rest

  let parse (msg : Cstruct.t) =
    try
      let count = Message.get_bitrates_count msg in
      let rec parse acc buf =
        match Cstruct.len buf with
        | 0 -> List.rev acc
        | _ ->
          let x, rest = of_stream_bitrate buf in
          parse (x :: acc) rest in
      let body = Cstruct.shift msg Message.sizeof_bitrates in
      Ok (if count > 0 then parse [] body else [])
    with Invalid_argument _ -> Error Request.Invalid_payload
end

module T2MI_info_parser = struct

  let parse_packets (buf : Cstruct.t) =
    let iter =
      Cstruct.iter
        (fun _ -> Some 1)
        (fun x -> int8_to_bool_list @@ Cstruct.get_uint8 x 0)
        (Message.get_t2mi_info_packets buf) in
    Cstruct.fold (fun acc el -> (List.rev el) @ acc) iter []
    |> List.rev
    |> List.fold_left (fun (i, acc) x ->
        if x then succ i, (i :: acc)
        else succ i, acc) (0, [])
    |> snd

  let parse_l1 (body : Cstruct.t) =
    let conf_len =
      Message.get_t2mi_info_ext_conf_len body
      |> fun x ->
      let r, d = x mod 8, x / 8 in
      d + (if r > 0 then 1 else 0) in
    let conf = snd @@ Cstruct.split body Message.sizeof_t2mi_info_ext in
    let conf = fst @@ Cstruct.split conf conf_len in
    let l1_pre' = Cstruct.to_string @@ Message.get_t2mi_info_ext_l1_pre body in
    let l1_post' = Cstruct.to_string conf in
    match L1_parser.l1_pre_of_string l1_pre' with
    | None -> None
    | Some l1_pre ->
      match L1_parser.l1_post_conf_of_string l1_pre l1_post' with
      | None -> None
      | Some x -> Some { T2mi_info. l1_pre; l1_post_conf = x }

  let parse stream (msg : Cstruct.t) =
    try
      let header, rest = Cstruct.split msg Message.sizeof_t2mi_info in
      let packets = parse_packets header in
      let sid = Message.get_t2mi_info_stream_id header in
      let length = Message.get_t2mi_info_length header in
      match length with
      | 0 ->
        let info =
          { T2mi_info.
            packets
          ; t2mi_pid = None
          ; l1 = None
          ; l1_empty = true
          ; l1_parse_error = false
          } in
        Ok (stream, (sid, info))
      | length ->
        let body, _ = Cstruct.split rest length in
        let l1 = parse_l1 body in
        let t2mi_pid = Some (Message.get_t2mi_info_ext_t2mi_pid body) in
        let l1_parse_error = match l1 with None -> true | Some _ -> false in
        let info =
          { T2mi_info.
            packets
          ; t2mi_pid
          ; l1
          ; l1_empty = false
          ; l1_parse_error
          } in
        Ok (stream, (sid, info))
    with Invalid_argument _ -> Error Request.Invalid_payload
end

(* let int_to_bool_list x =
 *   List.map (fun i -> (x land Int.pow 2 i) > 0) (List.range 0 7)
 * 
 * let int_to_t2mi_sync_list x =
 *   int_to_bool_list x
 *   |> List.foldi (fun acc i x -> if x then i :: acc else acc) []
 * 
 * let to_mode_exn mode t2mi_pid stream_id : input * t2mi_mode_raw =
 *   Option.get_exn @@ input_of_int (mode land 1),
 *   { enabled = if (mode land 4) > 0 then true else false
 *   ; pid = t2mi_pid land 0x1fff
 *   ; t2mi_stream_id = (t2mi_pid lsr 13) land 0x7
 *   ; stream = stream_id
 *   }
 * 
 * module Get_board_mode = struct
 * 
 *   let parse _ msg =
 *     to_mode_exn (get_board_mode_mode msg)
 *       (get_board_mode_t2mi_pid msg)
 *       (Multi_TS_ID.of_int32_pure (get_board_mode_stream_id msg))
 * end
 * 
 * module Get_jitter = struct
 * 
 *   let parse_item el packet_time : Jitter.measure =
 *     let status = get_jitter_item_status el in
 *     let d_pack = get_jitter_item_d_packet el in
 *     let d_pcr = get_jitter_item_d_pcr el in
 *     let drift = get_jitter_item_drift el in
 *     let fo = get_jitter_item_fo el in
 *     let jitter = get_jitter_item_jitter el in
 *     let t_pcr = ((Int32.to_float d_pcr) *. 10e+9) /. 27e+6 in
 *     let t_pcr_br = (Int32.to_float packet_time) *. (float_of_int d_pack) in
 *     let accuracy = t_pcr /. t_pcr_br in
 *     { discont_err = status land 0x8000 <> 0
 *     ; discont_ok = status land 0x4000 <> 0
 *     ; t_pcr = t_pcr_br
 *     ; accuracy
 *     ; jitter
 *     ; drift = Int32.float_of_bits drift
 *     ; fo = Int32.float_of_bits fo
 *     ; period = t_pcr_br /. 10e+6
 *     }
 * 
 *   let parse _ msg : jitter_raw =
 *     let hdr, bdy' = Cstruct.split msg sizeof_jitter in
 *     let count  = get_jitter_count hdr in
 *     let bdy, _  = Cstruct.split bdy' @@ sizeof_jitter_item * count in
 *     let pid = get_jitter_pid hdr in
 *     let t_pcr = Int32.float_of_bits @@ get_jitter_t_pcr hdr in
 *     let time = Int32.to_int @@ get_jitter_time hdr in
 *     let next_ptr = get_jitter_req_next hdr in
 *     let packet_time = get_jitter_packet_time hdr in
 *     let iter = Cstruct.iter (fun _ -> Some sizeof_jitter_item) (fun buf -> buf) bdy in
 *     let timestamp = Ptime_clock.now () in
 *     let measures =
 *       Cstruct.fold (fun acc el -> (parse_item el packet_time) :: acc)
 *         iter [] |> List.rev in
 *     { measures; next_ptr; time; timestamp; pid; t_pcr }
 * end
 *)
(*
 * module TS_streams = struct
 * 
 *   let parse msg =
 *     let hdr, bdy' = Cstruct.split msg sizeof_streams_list_event in
 *     let count = get_streams_list_event_count hdr in
 *     let bdy, _ = Cstruct.split bdy' (count * 4) in
 *     let iter =
 *       Cstruct.iter (fun _ -> Some 4)
 *         (fun buf -> Multi_TS_ID.of_int32_pure
 *           @@ Cstruct.LE.get_uint32 buf 0) bdy in
 *     List.rev @@ Cstruct.fold (fun acc el -> el :: acc) iter []
 * 
 * end
 * 
 * module Ts_errors = struct
 * 
 *   let prioriry_of_err_code = function
 *     | x when x >= 0x11 && x <= 0x16 -> 1
 *     | x when x >= 0x21 && x <= 0x26 -> 2
 *     | x when x >= 0x31 && x <= 0x38 -> 3
 *     | _ -> 0 (\* Unknown *\)
 * 
 *   let compare = fun x y ->
 *     Int32.compare x.packet y.packet
 * 
 *   let parse (msg : Cstruct.t) : Multi_TS_ID.t * (t list) =
 *     let common, rest = Cstruct.split msg sizeof_ts_errors in
 *     let number = get_ts_errors_count common in
 *     let errors, _ = Cstruct.split rest (number * sizeof_ts_error) in
 *     let stream_id = Multi_TS_ID.of_int32_pure
 *       @@ get_ts_errors_stream_id common in
 *     let iter = Cstruct.iter (fun _ -> Some sizeof_ts_error) (fun x -> x) errors in
 *     Cstruct.fold (fun acc el ->
 *         let pid' = get_ts_error_pid el in
 *         let pid = pid' land 0x1FFF in
 *         let err_code = get_ts_error_err_code el in
 *         { count = get_ts_error_count el
 *         ; err_code
 *         ; err_ext = get_ts_error_err_ext el
 *         ; priority = prioriry_of_err_code err_code
 *         ; multi_pid = (pid' land 0x8000) > 0
 *         ; pid
 *         ; packet = get_ts_error_packet el
 *         ; param_1 = get_ts_error_param_1 el
 *         ; param_2 = get_ts_error_param_2 el
 *         ; time = Time.epoch
 *         } :: acc) iter []
 *     |> List.sort compare
 *     |> Pair.make stream_id
 * 
 * end
 * 
 * module T2mi_errors = struct
 * 
 *   let ts_parser_error_code = 0xF0
 *   let t2mi_parser_error_code = 0xF1
 * 
 *   let get_relevant_t2mi_adv_code = function
 *     | 0 -> Some 2 | 1 -> Some 3 | 4 -> Some 4
 *     | 5 -> Some 5 | 6 -> Some 6 | 9 -> Some 7
 *     | 20 -> Some 8 | _ -> None
 * 
 *   let make_error ~count ~err_code ~param_1 ~param_2 ~pid () =
 *     { count
 *     ; err_code
 *     ; err_ext = 0
 *     ; priority = 0
 *     ; multi_pid = false
 *     ; pid
 *     ; packet = 0l
 *     ; param_1
 *     ; param_2
 *     ; time = Time.epoch
 *     }
 * 
 *   (\* Merge t2mi errors with counter and advanced errors.
 *      Result is common t2mi error type *\)
 *   let merge pid
 *       (count : t2mi_error_raw list)
 *       (param : t2mi_error_adv_raw list) : t list =
 *     List.map (fun (x : t2mi_error_raw) ->
 *         let open Option in
 *         let param =
 *           get_relevant_t2mi_adv_code x.code
 *           |> flat_map (fun c ->
 *               List.find_opt (fun a ->
 *                   a.code = c
 *                   && a.stream_id = x.stream_id) param
 *               |> map (fun (x : t2mi_error_adv_raw) -> Int32.of_int x.param))
 *           |> get_or ~default:0l
 *         in
 *         make_error ~count:x.count
 *           ~err_code:x.code
 *           ~pid
 *           ~param_1:param
 *           ~param_2:(Int32.of_int x.stream_id)
 *           ()) count
 * 
 *   (\* Convert t2mi advanced errors with 'code' = 0 to common t2mi error type *\)
 *   let convert_other pid (other : t2mi_error_adv_raw list) : t list =
 *     List.map (fun (x : t2mi_error_adv_raw) ->
 *         make_error ~count:1
 *           ~err_code:t2mi_parser_error_code
 *           ~pid
 *           ~param_1:(Int32.of_int x.param)
 *           ~param_2:(Int32.of_int x.stream_id)
 *           ()) other
 * 
 *   (\* Convert ts parser flags to common t2mi error type *\)
 *   let convert_ts pid (ts : int list) : t list =
 *     List.map (fun (x : int) ->
 *         make_error ~count:1
 *           ~err_code:ts_parser_error_code
 *           ~pid
 *           ~param_1:(Int32.of_int x)
 *           ~param_2:0l
 *           ()) ts
 * 
 *   let parse (msg : Cstruct.t) : Multi_TS_ID.t * (t list) =
 *     let common, rest = Cstruct.split msg sizeof_t2mi_errors in
 *     let number = get_t2mi_errors_count common in
 *     let errors, _ = Cstruct.split rest (number * sizeof_t2mi_error) in
 *     let stream_id = get_t2mi_errors_stream_id common in
 *     let pid = get_t2mi_errors_pid common in
 *     (\* let sync = int_to_t2mi_sync_list (get_t2mi_errors_sync common) in *\)
 *     let iter = Cstruct.iter (fun _ -> Some sizeof_t2mi_error)
 *         (fun buf -> buf) errors in
 *     let cnt, adv, oth =
 *       Cstruct.fold (fun (cnt, adv, oth) el ->
 *           let index = get_t2mi_error_index el in
 *           let data = get_t2mi_error_data el in
 *           let code = index lsr 4 in
 *           let sid = index land 7 in
 *           match index land 8 with
 *           | 0 -> if data > 0 (\* filter zero errors *\)
 *             then
 *               let (x : t2mi_error_raw) =
 *                 { code; stream_id = sid; count = data } in
 *               (x :: cnt), adv, oth
 *             else cnt,adv,oth
 *           | _ -> let f x = { code = x; stream_id = sid; param = data } in
 *             if code = 0 (\* t2mi parser error *\)
 *             then cnt, adv, ((f t2mi_parser_error_code) :: oth)
 *             else cnt, ((f code) :: adv), oth)
 *         iter ([], [], [])
 *     in
 *     let pe = get_t2mi_errors_err_flags common in
 *     let ts = List.filter_map (fun x ->
 *         if pe land (Int.pow 2 x) <> 0
 *         then Some x else None)
 *         (List.range 0 3) in
 *     let errors =
 *       merge pid cnt adv
 *       @ convert_other pid oth
 *       @ convert_ts pid ts in
 *     Multi_TS_ID.of_int32_pure stream_id, errors
 * 
 * end *)

let check_prefix (buf : Cstruct.t) =
  try
    let prefix = Message.get_common_header_prefix buf in
    if Message.prefix <> prefix
    then Error (Bad_prefix prefix)
    else Ok buf
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_msg_code (buf : Cstruct.t) =
  try
    let code = Message.get_common_header_msg_code buf in
    let has_crc = (code land 2) > 0 in
    match Request.rsp_tag_of_enum @@ code lsr 8 with
    | None -> Error (Bad_msg_code code)
    | Some tag ->
      let message, rest = Request.split_message has_crc buf tag in
      Ok (has_crc, code, tag, message, rest, buf)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_crc (has_crc, code, tag, message, rest, buf) =
  try
    if not has_crc
    then
      let body = Cstruct.shift message Message.sizeof_common_header in
      Ok (tag, body, rest)
    else
      let body', crc' = Cstruct.split message (Cstruct.len message - 2) in
      let iter =
        Cstruct.iter
          (fun _ -> Some 2)
          (fun buf -> Cstruct.LE.get_uint16 buf 0)
          (Cstruct.shift body' 2) in
      let crc = 0xFFFF land Cstruct.fold ( + ) iter 0 in
      let crc' = Cstruct.LE.get_uint16 crc' 0 in
      if crc <> crc'
      then Error (Bad_crc (code, crc, crc'))
      else
        let body = Cstruct.shift body' Message.sizeof_common_header in
        Ok (tag, body, rest)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let get_msg buf =
  let ( >>= ) x f = match x with Ok x -> f x | Error e -> Error e in
  check_prefix buf
  >>= check_msg_code
  >>= check_crc

let parse_part src data =
  let code_ext = Message.get_complex_rsp_header_code_ext data in
  let code = code_ext land 0x0FFF in
  let long = code_ext land 0x2000 <> 0 in
  let parity = if code_ext land 0x1000 <> 0 then 1 else 0 in
  let first = code_ext land 0x8000 <> 0 in
  let client_id = Message.get_complex_req_header_client_id data in
  let request_id = Message.get_complex_rsp_header_request_id data in
  let data' =
    if long
    then Cstruct.shift data Message.sizeof_complex_rsp_header_ext
    else Cstruct.shift data Message.sizeof_complex_rsp_header in
  let data, _ = Cstruct.(split data' (len data' - parity)) in
  let param =
    Int32.mul 2l
      (if long
       then Message.get_complex_rsp_header_ext_param data
       else Int32.of_int @@ Message.get_complex_rsp_header_param data)
    |> fun x -> Int32.sub x (Int32.of_int parity) in
  Logs.debug ~src (fun m ->
      m "parser - got complex message part \
         (first: %B, code: 0x%X, parity: %d, \
         req_id: %d, client_id: %d, \
         length: %d, param: %ld)"
        first code parity request_id client_id
        (Cstruct.len data) param);
  let id = { code; client_id; request_id } in
  let part = { data; first; param } in
  List.Assoc.update ~eq:equal_part_id (function
      | Some x -> Some (part :: x)
      | None -> Some [part]) id

let parse src data parts = function
  | `Part -> None, parse_part src data parts
  | tag -> Some Request.{ tag; data }, parts

(* let parse_complex_msg = fun ((code, r_id), (msg : Cstruct.t)) ->
 *   try
 *     let data = (r_id, msg) in
 *     (match code with
 *      | x when x = Get_board_errors.code ->
 *        `ER (`Board_errors data)
 *      | x when x = Get_section.code ->
 *        `R (`Section data)
 *      | x when x = Get_t2mi_frame_seq.code ->
 *        `R (`T2mi_frame_seq data)
 *      | x when x = Get_jitter.code ->
 *        `ER (`Jitter (r_id, (get_jitter_req_ptr msg), msg))
 *      | x when x = Get_ts_struct.code ->
 *        `ER (`Struct (r_id, (get_ts_structs_version msg), msg))
 *      | x when x = Get_bitrate.code ->
 *        `ER (`Bitrates (r_id, (get_bitrates_version msg), msg))
 *      | x when x = Get_t2mi_info.code ->
 *        `ER (`T2mi_info (r_id,
 *                         (get_t2mi_info_version msg),
 *                         (get_t2mi_info_stream_id msg),
 *                         msg))
 *      | _ -> Logs.debug (fun m ->
 *          m "parser - unknown complex message code: 0x%x" code); `N)
 *   with e ->
 *     Logs.warn (fun m ->
 *         m "parser - failure while parsing complex message: %s"
 *           (Printexc.to_string e)); `N *)

(* FIXME optimize, we should not try to compose parts once again if failed,
   store already composed items *)
let try_compose_parts src ((id, acc) as x) =
  let sorted = List.sort (fun x y ->
      if x.first then (-1)
      else if y.first then 1
      else compare x.param y.param) acc in
  match sorted with
  | [] -> `P x
  | first :: rest ->
    try
      let acc =
        List.fold_left (fun acc x ->
            if Int32.equal x.param (Int32.of_int (Cstruct.len acc))
            then Cstruct.append acc x.data
            else failwith "Incorrect part offset")
          first.data rest
      in
      if Int32.equal first.param (Int32.of_int (Cstruct.len acc))
      then `F (id, acc)
      else `P x
    with e ->
      Logs.warn ~src (fun m ->
          m "parser - failure while composing complex message parts: %s"
          @@ Printexc.to_string e);
      `N

let deserialize src parts buf =
  let rec f acc parts buf =
    if Cstruct.len buf < Message.sizeof_common_header
    then List.(rev acc, rev parts, buf)
    else
      match get_msg buf with
      | Ok (tag, body, rest) ->
        begin match parse src body parts tag with
          | Some rsp, parts -> f (rsp :: acc) parts rest
          | None, parts -> f acc parts rest
        end
      | Error e ->
        begin match e with
          | Insufficient_payload x -> List.(rev acc, rev parts, x)
          | e ->
            Logs.warn ~src (fun m -> m "parser error: %s" @@ error_to_string e);
            f acc parts (Cstruct.shift buf 1)
        end in
  let responses, parts, res = f [] parts buf in
  let parts =
    List.filter (fun (_, x) ->
        let first_msgs = List.find_all (fun x -> x.first) x in
        match first_msgs with
        | [_] -> true
        | _ -> false) parts in
  (* let ev_rsps, rsps, parts =
   *   List.fold_left (fun ((e, r, p) as acc) x ->
   *       match try_compose_parts src x with
   *       | `F x ->
   *         begin match parse_complex_msg x with
   *           | `ER x -> x :: e, r, p
   *           | `R x -> e, x :: r, p
   *           | `N -> acc
   *         end
   *       | `P x -> e, r, x :: p
   *       | `N -> e, r, p)
   *     (ev_rsps, rsps, []) parts
   * in *)
  responses, parts, if Cstruct.len res > 0 then Some res else None

let parse_mode _ = Error Request.Invalid_payload
let parse_jitter _ = Error Request.Invalid_payload

let is_response (type a) (req : a Request.t)
    (msg : Request.rsp) : (a, Request.error) result option =
  match req with
  | Get_devinfo ->
    (match msg with
     | `Simple { tag = `Devinfo; data } -> Some (parse_devinfo data)
     | _ -> None)
  | Get_deverr id ->
    (match msg with
     | `Complex { tag = `Deverr; data; request_id; _ } when id = request_id ->
       Some (Deverr_parser.parse data)
     | _ -> None)
  | Get_mode ->
    (match msg with
     | `Simple { tag = `Mode; data } -> Some (parse_mode data)
     | _ -> None)
  | Set_mode _ -> None
  | Set_jitter_mode _ -> None
  | Reset -> None
  | Set_src_id _ -> None
  | Get_t2mi_seq { request_id = id; _ } ->
    (match msg with
     | `Complex { tag = `T2mi_seq; data; request_id; _ } when id = request_id ->
       Some (parse_t2mi_sequence data)
     | _ -> None)
  | Get_section { request_id = id; table_id; section; _ } ->
    (match msg with
     | `Complex { tag = `Section; data; request_id; _ } when id = request_id ->
       Some (parse_section ~table_id ~section data)
     | _ -> None)
  | Get_jitter { request_id = id; _ } ->
    (match msg with
     | `Complex { tag = `Jitter; data; request_id; _ } when id = request_id ->
       Some (parse_jitter data)
     | _ -> None)
  | Get_bitrate id ->
    (match msg with
     | `Complex { tag = `Bitrate; data; request_id; _ } when id = request_id ->
       Some (Bitrate_parser.parse data)
     | _ -> None)
  | Get_structure { request_id = id; stream } ->
    (match msg with
     | `Complex { tag = `Structure; data; request_id; _ } when id = request_id ->
       Some (Structure_parser.parse stream data)
     | _ -> None)
  | Get_t2mi_info { request_id = id; stream; _ } ->
    (match msg with
     | `Complex { tag = `T2mi_info; data; request_id; _ } when id = request_id ->
       Some (T2MI_info_parser.parse stream data)
     | _ -> None)
