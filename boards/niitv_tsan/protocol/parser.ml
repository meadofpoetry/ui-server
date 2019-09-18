open Application_types
open Board_niitv_tsan_types

type error =
  | Bad_prefix of int
  | Bad_length of int
  | Bad_msg_code of int
  | Bad_crc of int * int * int
  | Insufficient_payload of Cstruct.t

let error_to_string = function
  | Bad_prefix x -> "invalid prefix: " ^ string_of_int x
  | Bad_length x -> "invalid length: " ^ string_of_int x
  | Bad_msg_code x -> "invalid code: " ^ string_of_int x
  | Insufficient_payload _ -> "insufficient payload"
  | Bad_crc (code, x, y) ->
      Printf.sprintf
        "invalid CRC in message with code = 0x%x,expected %d, got %d"
        code
        x
        y

module Part_id = struct
  type t =
    { code : int
    ; request_id : Uint16.t
    ; client_id : Uint16.t }
  [@@deriving eq, ord]
end

module Part = Map.Make (Part_id)

type part =
  { first : bool
  ; param : int32
  ; parity : bool
  ; body : Cstruct.t }

type part_error =
  | Unexpected_first
  | Unexpected_parity
  | Discontinuity

exception Invalid_part of part_error

let pp_part_error ppf = function
  | Unexpected_first -> Format.pp_print_string ppf "several parts have 'first' flag set"
  | Unexpected_parity ->
      Format.pp_print_string ppf "different value of 'parity' flag across parts"
  | Discontinuity -> Format.pp_print_string ppf "unfilled gap between parts"

let compare_part x y =
  if x.first then -1 else if y.first then 1 else compare x.param y.param

let non_zero_indexes_of_int ?(start = 0) ~n (v : int) =
  let n = min Sys.int_size n in
  let rec loop acc = function
    | i when i = n -> acc
    | i ->
        let acc = if v land (1 lsl i) > 0 then (start + i) :: acc else acc in
        loop acc (succ i)
  in
  loop [] 0

let make_mode mode t2mi_pid stream =
  ( input_of_enum (mode land 1)
  , { enabled = (if mode land 4 > 0 then true else false)
    ; pid = t2mi_pid land 0x1fff
    ; t2mi_stream_id = (t2mi_pid lsr 13) land 0x7
    ; stream = ID stream } )

let parse_devinfo (msg : Cstruct.t) =
  try
    Ok
      { typ = Message.get_board_info_board_type msg
      ; ver = Message.get_board_info_board_version msg }
  with _ -> Error Request.Invalid_payload

let parse_mode (msg : Cstruct.t) =
  let input, t2mi_mode =
    make_mode
      (Message.get_board_mode_mode msg)
      (Message.get_board_mode_t2mi_pid msg)
      (Stream.Multi_TS_ID.of_int32_pure @@ Message.get_board_mode_stream_id msg)
  in
  match input with
  | None -> Error Request.Invalid_payload
  | Some i -> Ok (i, t2mi_mode)

let parse_streams (msg : Cstruct.t) =
  try
    let header, body' = Cstruct.split msg Message.sizeof_streams_list_event in
    let count = Message.get_streams_list_event_count header in
    let body, _ = Cstruct.split body' (count * 4) in
    let iter =
      Cstruct.iter
        (fun _ -> Some 4)
        (fun x -> Stream.Multi_TS_ID.of_int32_pure @@ Cstruct.LE.get_uint32 x 0)
        body
    in
    Ok (List.rev @@ Cstruct.fold (fun acc el -> el :: acc) iter [])
  with Invalid_argument _ -> Error Request.Invalid_payload

module Status = struct
  type versions =
    { status : int
    ; streams : int
    ; ts_common : int
    ; ts : int list
    ; t2mi : (int * int) list }
  [@@deriving eq, show]

  type t =
    { basic : status
    ; input : input
    ; t2mi_mode : t2mi_mode
    ; jitter_mode : jitter_mode
    ; errors : bool
    ; t2mi_sync : int list
    ; versions : versions }
  [@@deriving eq, show]

  let parse_t2mi_versions (buf : Cstruct.t) =
    let v = Message.get_status_t2mi_ver_lst buf in
    let rec aux acc = function
      | 8 -> acc
      | i ->
          let ver = Int32.shift_right v (4 * i) |> Int32.logand 0xfl |> Int32.to_int in
          let acc = (i, ver) :: acc in
          aux acc (succ i)
    in
    aux [] 0

  let parse_versions ts_num (buf : Cstruct.t) : versions =
    let iter =
      Cstruct.iter (fun _ -> Some 1) (fun buf -> Cstruct.get_uint8 buf 0)
      @@ Message.get_status_ts_ver_lst buf
    in
    let ts =
      Cstruct.fold (fun acc el -> el :: acc) iter []
      |> List.rev
      |> Boards.Util.List.take ts_num
    in
    { status = Message.get_status_version buf
    ; streams = Message.get_status_streams_ver buf
    ; ts_common = Message.get_status_ts_ver_com buf
    ; ts
    ; t2mi = parse_t2mi_versions buf }

  let parse ?(timestamp = Ptime_clock.now ()) (msg : Cstruct.t) =
    let flags = Message.get_status_flags msg in
    let ts_num = Message.get_status_ts_num msg in
    let flags2 = Message.get_status_flags_2 msg in
    let has_sync = flags land 0x04 = 0 in
    let ts_num = if has_sync then ts_num else 0 in
    let packet_sz =
      if flags land 0x08 <> 0
      then Ts192
      else if flags land 0x10 <> 0
      then Ts204
      else Ts188
    in
    let t2mi_sync = non_zero_indexes_of_int ~n:8 (Message.get_status_t2mi_sync msg) in
    let input, t2mi_mode =
      make_mode
        (Message.get_status_mode msg)
        (Message.get_status_t2mi_pid msg)
        (Stream.Multi_TS_ID.of_int32_pure @@ Message.get_status_t2mi_stream_id msg)
    in
    let jitter_mode =
      let pid = Message.get_status_jitter_pid msg in
      let id = Message.get_status_jitter_stream_id msg in
      {pid; stream = Stream.Multi_TS_ID.of_int32_pure id; stream_id = None}
    in
    let (basic : status) =
      { timestamp
      ; load = float_of_int (Message.get_status_load msg * 100) /. 255.
      ; reset = flags2 land 0x02 <> 0
      ; ts_num
      ; services_num = Message.get_status_services_num msg
      ; bitrate = Int32.to_int @@ Message.get_status_bitrate msg
      ; packet_sz
      ; has_sync
      ; has_stream = flags land 0x80 = 0 }
    in
    match input with
    | None -> Error Request.Invalid_payload
    | Some input ->
        Ok
          { basic
          ; input
          ; t2mi_mode
          ; jitter_mode
          ; errors = flags land 0x20 <> 0
          ; t2mi_sync
          ; versions = parse_versions ts_num msg }
end

module Deverr = struct
  let param_codes = [18; 32]

  let traverse timestamp (code, acc) item =
    let is_param = List.exists (fun x -> x = code) param_codes in
    let acc =
      if not is_param
      then
        let item =
          { Deverr.timestamp
          ; source = Hardware
          ; code
          ; count = Int32.to_int item
          ; param = None }
        in
        item :: acc
      else
        match acc with
        | hd :: tl -> {hd with param = Some (Int32.to_int item)} :: tl
        | _ -> acc
    in
    succ code, acc

  let parse ?(timestamp = Ptime_clock.now ()) (msg : Cstruct.t) =
    let iter =
      Cstruct.iter
        (fun _ -> Some 4)
        (fun buf -> Cstruct.LE.get_uint32 buf 0)
        (Message.get_board_errors_errors msg)
    in
    try Ok (List.rev @@ snd @@ Cstruct.fold (traverse timestamp) iter (0, []))
    with Invalid_argument _ -> Error Request.Invalid_payload
end

module Section = struct
  let parse
      ?(timestamp = Ptime_clock.now ())
      ~(table_id : int)
      ~(section : int option)
      (msg : Cstruct.t) =
    try
      let hdr, bdy = Cstruct.split msg Message.sizeof_section in
      let length = Message.get_section_length hdr in
      let result = Message.get_section_result hdr in
      if length > 0 && result = 0
      then
        let sid, data = Cstruct.split bdy 4 in
        let stream_id = Cstruct.LE.get_uint32 sid 0 in
        let raw = Cstruct.to_string data in
        let section_id =
          match section with
          | None -> 0
          | Some x -> x
        in
        let data =
          { SI_PSI_section.Dump.section = Base64.encode_exn raw
          ; stream_id = Stream.Multi_TS_ID.of_int32_pure stream_id
          ; table_id
          ; section_id
          ; content =
              (match Si_psi_parser.parse raw with
              | Ok x -> Some x
              | Error _ -> None) }
        in
        Ok {timestamp; data}
      else
        let error =
          match result with
          | 0 | 3 -> Request.Custom "Section is empty"
          | 1 -> Custom "Table not found"
          | 2 -> Custom "Section not found"
          | 4 -> Custom "Stream_not_found"
          | x -> Custom (Printf.sprintf "Unknown error code %d" x)
        in
        Error error
    with Invalid_argument _ -> Error Request.Invalid_payload
end

module T2MI_sequence = struct
  let parse ?(timestamp = Ptime_clock.now ()) (msg : Cstruct.t) =
    let iter =
      Cstruct.iter (fun _ -> Some Message.sizeof_t2mi_frame_seq_item) (fun x -> x) msg
    in
    try
      let data =
        List.rev
        @@ Cstruct.fold
             (fun (acc : T2mi_sequence.item list) el ->
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
               ; ts_packet }
               :: acc)
             iter
             []
      in
      Ok {timestamp; data}
    with Invalid_argument _ -> Error Request.Invalid_payload
end

module Structure = struct
  let ( % ) f g x = f (g x)

  type service_acc =
    { info : Cstruct.t
    ; es : Cstruct.t
    ; ecm : Cstruct.t }

  type acc =
    { general : Cstruct.t
    ; pids : Cstruct.t
    ; services : service_acc list
    ; emm : Cstruct.t
    ; tables : Cstruct.t list }

  let service_acc_empty = {info = Cstruct.empty; es = Cstruct.empty; ecm = Cstruct.empty}

  let acc_empty : acc =
    { general = Cstruct.empty
    ; pids = Cstruct.empty
    ; services = []
    ; emm = Cstruct.empty
    ; tables = [] }

  let of_general_block (msg : Cstruct.t) =
    let bdy, rest = Cstruct.split msg Message.sizeof_general_struct_block in
    let string_len = Message.get_general_struct_block_string_len bdy in
    let nw_pid' = Message.get_general_struct_block_network_pid bdy in
    let strings, _ = Cstruct.split rest (string_len * 2) in
    let nw_name, bq_name = Cstruct.split strings string_len in
    let nw_name =
      match Text_decoder.decode nw_name with
      | Error _ -> ""
      | Ok s -> s
    in
    let bouquet_name =
      match Text_decoder.decode bq_name with
      | Error _ -> ""
      | Ok s -> s
    in
    ( { TS_info.services_num = Message.get_general_struct_block_services_num bdy
      ; nw_pid = nw_pid' land 0x1FFF
      ; complete = nw_pid' land 0x4000 <> 0
      ; ts_id = Message.get_general_struct_block_ts_id bdy
      ; nw_id = Message.get_general_struct_block_nw_id bdy
      ; orig_nw_id = Message.get_general_struct_block_orig_nw_id bdy
      ; nw_name
      ; bouquet_name }
    , string_len )

  let of_pids_block (msg : Cstruct.t) =
    let iter = Cstruct.iter (fun _ -> Some 2) (fun x -> Cstruct.LE.get_uint16 x 0) msg in
    Cstruct.fold
      (fun acc el ->
        let pid = el land 0x1FFF in
        let info =
          { PID.has_pts = el land 0x8000 <> 0
          ; has_pcr = false (* Filled in later *)
          ; scrambled = el land 0x4000 <> 0
          ; typ = Private (* Filled in later *)
          ; service_id = None (* Filled in later *)
          ; service_name = None
          ; present = el land 0x2000 <> 0 }
        in
        (pid, info) :: acc)
      iter
      []
    |> List.sort (fun (a, _) (b, _) -> compare a b)

  let of_service_block (string_len : int) (msg : Cstruct.t) =
    let bdy, rest = Cstruct.split msg Message.sizeof_services_struct_block in
    let flags = Message.get_services_struct_block_flags bdy in
    let strings, _ = Cstruct.split rest (string_len * 2) in
    let sn, pn = Cstruct.split strings string_len in
    let id = Message.get_services_struct_block_id bdy in
    let name =
      match Text_decoder.decode sn with
      | Error _ -> Printf.sprintf "Service %d" id
      | Ok s -> s
    in
    let provider_name =
      match Text_decoder.decode pn with
      | Error _ -> ""
      | Ok s -> s
    in
    let info =
      { Service.name
      ; provider_name
      ; pmt_pid = Message.get_services_struct_block_pmt_pid bdy
      ; pcr_pid = Message.get_services_struct_block_pcr_pid bdy
      ; has_pmt = flags land 0x8000 <> 0
      ; has_sdt = flags land 0x4000 <> 0
      ; dscr = flags land 0x2000 <> 0
      ; dscr_list = flags land 0x1000 <> 0
      ; eit_schedule = flags land 0x0080 <> 0
      ; eit_pf = flags land 0x0040 <> 0
      ; free_ca_mode = flags land 0x0020 <> 0
      ; running_status = flags land 0x0007
      ; service_type = Message.get_services_struct_block_service_type bdy
      ; service_type_list = Message.get_services_struct_block_service_type_list bdy
      ; elements = [] (* Filled in later *) }
    in
    id, info

  let of_es_block (msg : Cstruct.t) =
    let iter = Cstruct.iter (fun _ -> Some 4) (fun x -> x) msg in
    List.rev
    @@ Cstruct.fold
         (fun acc x ->
           let pid = Message.get_es_struct_block_pid x land 0x1FFF in
           let info =
             { MPEG_TS.PID.Type.stream_type = Message.get_es_struct_block_es_type x
             ; stream_id = Message.get_es_struct_block_es_stream_id x }
           in
           (pid, MPEG_TS.PID.Type.PES info) :: acc)
         iter
         []

  let of_ecm_block (msg : Cstruct.t) =
    let iter = Cstruct.iter (fun _ -> Some 4) (fun x -> x) msg in
    List.rev
    @@ Cstruct.fold
         (fun acc x ->
           let pid = Message.get_ecm_struct_block_pid x land 0x1FFF in
           let ca_sys_id = Message.get_ecm_struct_block_ca_system_id x in
           let info = MPEG_TS.PID.Type.ECM {ca_sys_id} in
           (pid, info) :: acc)
         iter
         []

  let of_emm_block (msg : Cstruct.t) = of_ecm_block msg

  let of_table_block (msg : Cstruct.t) =
    let bdy, rest = Cstruct.split msg Message.sizeof_table_struct_block in
    let iter =
      Cstruct.iter (fun _ -> Some 2) (fun buf -> Cstruct.LE.get_uint16 buf 0) rest
    in
    let sections =
      Cstruct.fold
        (fun acc x ->
          let section = List.length acc in
          (* let analyzed = (x land 0x8000) > 0 in *)
          let length = x land 0x0FFF in
          {SI_PSI_table.section; length} :: acc)
        iter
        []
      |> List.filter (fun {SI_PSI_table.length; _} -> length > 0)
      |> List.rev
    in
    let pid' = Message.get_table_struct_block_pid msg in
    let table_id = Message.get_table_struct_block_table_id bdy in
    let table_id_ext = Message.get_table_struct_block_table_id_ext bdy in
    let id_ext_1 = Message.get_table_struct_block_id_ext_1 bdy in
    let id_ext_2 = Message.get_table_struct_block_id_ext_2 bdy in
    let eit_segment_lsn = Message.get_table_struct_block_eit_segment_lsn bdy in
    let eit_last_table_id = Message.get_table_struct_block_eit_last_table_id bdy in
    let version = Message.get_table_struct_block_version bdy in
    let pid = pid' land 0x1FFF in
    let last_section = Message.get_table_struct_block_lsn bdy in
    let section_syntax = pid' land 0x8000 > 0 in
    let id = {SI_PSI_table.table_id; table_id_ext; id_ext_1; id_ext_2; section_syntax} in
    let info =
      { SI_PSI_table.version
      ; pid
      ; service_id = None (* Filled in later *)
      ; service_name = None
      ; last_section
      ; eit_segment_lsn
      ; eit_last_table_id
      ; sections }
    in
    id, info

  let split_into_blocks (msg : Cstruct.t) : acc =
    let rec aux service_flag msg acc =
      match Cstruct.len msg with
      | 0 -> acc
      | _ ->
          let hdr, data = Cstruct.split msg Message.sizeof_struct_block_header in
          let len = Message.get_struct_block_header_length hdr in
          let block, rest = Cstruct.split data len in
          let acc, flag =
            match Message.get_struct_block_header_code hdr with
            | 0x2000 -> {acc with general = block}, false
            | 0x2100 -> {acc with pids = Cstruct.append acc.pids block}, false
            | 0x2200 ->
                let service = {service_acc_empty with info = block} in
                {acc with services = service :: acc.services}, true
            | 0x2201 -> (
              match service_flag, acc.services with
              | false, _ | _, [] -> failwith "no service block before es block"
              | _, ({es; _} as hd) :: tl ->
                  let es = Cstruct.append es block in
                  {acc with services = {hd with es} :: tl}, true)
            | 0x2202 -> (
              match service_flag, acc.services with
              | false, _ | _, [] -> failwith "no service block before ecm block"
              | _, ({ecm; _} as hd) :: tl ->
                  let ecm = Cstruct.append ecm block in
                  {acc with services = {hd with ecm} :: tl}, true)
            | 0x2300 -> {acc with emm = Cstruct.append acc.emm block}, false
            | 0x2400 -> {acc with tables = block :: acc.tables}, false
            | x ->
                let s = Printf.sprintf "unknown structure block: 0x%X" x in
                failwith s
          in
          aux flag rest acc
    in
    aux false msg acc_empty

  let parse_service (slen : int) (acc : service_acc) =
    let es = of_es_block acc.es in
    let ecm = of_ecm_block acc.ecm in
    let elements = List.map fst @@ es @ ecm in
    let sid, sinfo = of_service_block slen acc.info in
    (sid, {sinfo with elements}), es @ ecm

  let update_if_null (pid, (info : PID.t)) =
    if pid <> 0x1FFF then None else Some (pid, {info with typ = Null})

  let find_in_elements ((pid : int), _) elements =
    let pid = List.find_opt (fun (pid', _) -> pid' = pid) elements in
    match pid with
    | None -> None
    | Some (_, info) -> Some info

  let update_if_in_services elements (pid, (info : PID.t)) =
    let ( >>= ) o f =
      match o with
      | None -> None
      | Some x -> f x
    in
    Boards.Util.List.find_map
      (fun ((sid, (sinfo : Service.t)), elts) ->
        match find_in_elements (pid, info) elts with
        | None -> None
        | Some t -> Some (sid, sinfo.name, sinfo.pcr_pid, t))
      elements
    >>= fun (id, name, pcr_pid, typ) ->
    let has_pcr = pid = pcr_pid in
    Some (pid, {info with service_id = Some id; service_name = Some name; typ; has_pcr})

  let update_if_in_emm emm (pid, (info : PID.t)) =
    match find_in_elements (pid, info) emm with
    | None -> None
    | Some typ -> Some (pid, {info with typ})

  let update_if_in_tables tables (pid, (info : PID.t)) =
    let ( >>= ) o f =
      match o with
      | None -> None
      | Some x -> f x
    in
    List.find_all (fun (_, (info : SI_PSI_table.t)) -> info.pid = pid) tables
    |> (function
         | [] -> None
         | [((id : SI_PSI_table.id), info)] -> (
             let open MPEG_TS.PID.Type in
             match MPEG_TS.SI_PSI.of_table_id id.table_id with
             | `PMT -> Some (info.service_id, info.service_name, SEC [id.table_id])
             | _ -> Some (None, None, SEC [id.table_id]))
         | l ->
             let ids =
               List.map (fun ((id : SI_PSI_table.id), _) -> id.table_id) l
               |> List.sort_uniq Pervasives.compare
             in
             Some (None, None, SEC ids))
    >>= fun (id, name, typ) ->
    Some (pid, {info with service_id = id; service_name = name; typ})

  let update_pid elements tables emm pid =
    let ( >>= ) x f =
      match x with
      | Some x -> Some x
      | None -> f pid
    in
    update_if_null pid
    >>= update_if_in_services elements
    >>= update_if_in_emm emm
    >>= update_if_in_tables tables
    |> function
    | None -> pid
    | Some x -> x

  let update_table services ((id : SI_PSI_table.id), (info : SI_PSI_table.t)) =
    (match MPEG_TS.SI_PSI.of_table_id id.table_id with
    | `EIT (`Actual, _) -> (
        let service = List.find_opt (fun (sid, _) -> sid = id.table_id_ext) services in
        match service with
        | None -> None
        | Some (id, (info : Service.t)) -> Some (id, info.name))
    | `PMT -> (
        let service =
          List.find_opt
            (fun (_, (sinfo : Service.t)) -> sinfo.has_pmt && sinfo.pmt_pid = info.pid)
            services
        in
        match service with
        | None -> None
        | Some (id, info) -> Some (id, info.name))
    | _ -> None)
    |> function
    | None -> id, info
    | Some (sid, name) -> id, {info with service_id = Some sid; service_name = Some name}

  let parse_blocks timestamp (msg : Cstruct.t) =
    let acc = split_into_blocks msg in
    let info, slen = of_general_block acc.general in
    let elements = List.map (parse_service slen) acc.services in
    let services =
      List.split elements |> fst |> List.sort (fun (a, _) (b, _) -> compare a b)
    in
    let tables = List.rev_map (update_table services % of_table_block) acc.tables in
    let emm = of_emm_block acc.emm in
    let pids = List.map (update_pid elements tables emm) @@ of_pids_block acc.pids in
    {Structure.info; services; tables; pids; timestamp}

  let of_ts_struct timestamp msg =
    let header, rest = Cstruct.split msg Message.sizeof_ts_struct in
    let length = Int32.to_int @@ Message.get_ts_struct_length header in
    let body, rest = Cstruct.split rest length in
    let structure = parse_blocks timestamp body in
    let stream =
      Stream.Multi_TS_ID.of_int32_pure @@ Message.get_ts_struct_stream_id header
    in
    (stream, structure), rest

  let parse ?(timestamp = Ptime_clock.now ()) stream (msg : Cstruct.t) =
    try
      let count = Message.get_ts_structs_count msg in
      (* stream id list *)
      let body = Cstruct.shift msg (Message.sizeof_ts_structs + (count * 4)) in
      (* FIXME stuffing bytes when requesting for a single stream *)
      let parse =
        match stream with
        | `All ->
            let rec aux acc buf =
              match Cstruct.len buf with
              | 0 -> List.rev acc
              | _ ->
                  let x, rest = of_ts_struct timestamp buf in
                  aux (x :: acc) rest
            in
            aux []
        | `Single _ ->
            fun buf ->
              let s, _ = of_ts_struct timestamp buf in
              [s]
      in
      Ok (if count > 0 then parse body else [])
    with Invalid_argument _ -> Error Request.Invalid_payload
end

module Bitrate = struct
  let of_pids_bitrate total_pids br_per_pkt (buf : Cstruct.t) =
    let msg, rest = Cstruct.split buf (Message.sizeof_pid_bitrate * total_pids) in
    let iter =
      Cstruct.iter (fun _ -> Some Message.sizeof_pid_bitrate) (fun x -> x) msg
    in
    let pids =
      Cstruct.fold
        (fun acc el ->
          let packets = Message.get_pid_bitrate_packets el in
          let pid = Message.get_pid_bitrate_pid el land 0x1FFF in
          let br = int_of_float @@ (br_per_pkt *. Int32.to_float packets) in
          (pid, br) :: acc)
        iter
        []
    in
    List.rev pids, rest

  let of_tables_bitrate (total_tbls : int) (br_per_pkt : float) (buf : Cstruct.t) =
    let msg, _ = Cstruct.split buf (Message.sizeof_table_bitrate * total_tbls) in
    let iter =
      Cstruct.iter (fun _ -> Some Message.sizeof_table_bitrate) (fun x -> x) msg
    in
    let tables =
      Cstruct.fold
        (fun acc el ->
          let packets = Message.get_table_bitrate_packets el in
          let flags = Message.get_table_bitrate_flags el in
          let id_ext_1 = Message.get_table_bitrate_id_ext_1 el in
          let id_ext_2 = Message.get_table_bitrate_id_ext_2 el in
          let rate = int_of_float @@ (br_per_pkt *. Int32.to_float packets) in
          let _fully_analyzed = flags land 2 > 0 in
          ( { SI_PSI_table.table_id = Message.get_table_bitrate_table_id el
            ; table_id_ext = Message.get_table_bitrate_table_id_ext el
            ; id_ext_1
            ; id_ext_2
            ; section_syntax = flags land 1 > 0 }
          , rate )
          :: acc)
        iter
        []
    in
    List.rev tables

  let of_stream_bitrate timestamp (buf : Cstruct.t) =
    let length = Int32.to_int @@ Message.get_stream_bitrate_length buf in
    let msg, rest = Cstruct.split buf (length + 8) in
    let hdr, bdy = Cstruct.split msg Message.sizeof_stream_bitrate in
    let total = Int32.to_int @@ Message.get_stream_bitrate_ts_bitrate hdr in
    let total_pkts = Message.get_stream_bitrate_total_packets hdr in
    let br_per_pkt = float_of_int total /. Int32.to_float total_pkts in
    let total_pids = Message.get_stream_bitrate_total_pids hdr in
    let total_tbls = Message.get_stream_bitrate_total_tables hdr in
    let pids, tbls = of_pids_bitrate total_pids br_per_pkt bdy in
    let stream = Message.get_stream_bitrate_stream_id hdr in
    let null = Option.value ~default:0 (List.assoc_opt 0x1FFF pids) in
    let data =
      { Bitrate.total
      ; effective = total - null
      ; pids
      ; tables = of_tables_bitrate total_tbls br_per_pkt tbls
      ; timestamp }
    in
    let rsp = Stream.Multi_TS_ID.of_int32_pure stream, data in
    rsp, rest

  let parse ?(timestamp = Ptime_clock.now ()) (msg : Cstruct.t) =
    try
      let count = Message.get_bitrates_count msg in
      let rec parse acc buf =
        match Cstruct.len buf with
        | 0 -> List.rev acc
        | _ ->
            let x, rest = of_stream_bitrate timestamp buf in
            parse (x :: acc) rest
      in
      let body = Cstruct.shift msg Message.sizeof_bitrates in
      Ok (if count > 0 then parse [] body else [])
    with Invalid_argument _ -> Error Request.Invalid_payload
end

module T2MI_info = struct
  let parse_packets (buf : Cstruct.t) =
    let iter =
      Cstruct.iter
        (fun _ -> Some 1)
        (fun x -> Cstruct.get_uint8 x 0)
        (Message.get_t2mi_info_packets buf)
    in
    Cstruct.fold
      (fun (i, acc) el ->
        let l = non_zero_indexes_of_int ~start:i ~n:8 el in
        i + 8, l @ acc)
      iter
      (0, [])
    |> snd

  let parse_l1 (body : Cstruct.t) =
    try
      let conf_len =
        Message.get_t2mi_info_ext_conf_len body
        |> fun x ->
        let r, d = x mod 8, x / 8 in
        d + if r > 0 then 1 else 0
      in
      let conf =
        Cstruct.split body Message.sizeof_t2mi_info_ext
        |> snd
        |> (fun conf -> Cstruct.split conf conf_len)
        |> fst
      in
      let l1_pre =
        L1_parser.parse_l1_pre
        @@ Bitstring.bitstring_of_string
        @@ Cstruct.to_string
        @@ Message.get_t2mi_info_ext_l1_pre body
      in
      let l1_post_conf =
        L1_parser.parse_l1_post_conf ~s2:l1_pre.s2 ~num_rf:l1_pre.num_rf
        @@ Bitstring.bitstring_of_string
        @@ Cstruct.to_string conf
      in
      Ok {T2mi_info.l1_pre; l1_post_conf}
    with
    | Invalid_argument _ -> Error Request.Invalid_payload
    | Match_failure _ -> Error Request.Invalid_payload

  let parse ?(timestamp = Ptime_clock.now ()) (msg : Cstruct.t) =
    try
      let header, rest = Cstruct.split msg Message.sizeof_t2mi_info in
      let packets = parse_packets header in
      let t2mi_stream_id = Message.get_t2mi_info_stream_id header in
      let length = Message.get_t2mi_info_length header in
      match length with
      | 0 ->
          let info = {T2mi_info.packets; t2mi_pid = None; l1 = None; timestamp} in
          Ok (t2mi_stream_id, info)
      | length -> (
          let body = fst @@ Cstruct.split rest length in
          match parse_l1 body with
          | Error _ as e -> e
          | Ok l1 ->
              let info =
                { T2mi_info.packets
                ; t2mi_pid = Some (Message.get_t2mi_info_ext_t2mi_pid body)
                ; l1 = Some l1
                ; timestamp }
              in
              Ok (t2mi_stream_id, info))
    with Invalid_argument _ -> Error Request.Invalid_payload
end

module TS_error = struct
  let parse ?(timestamp = Ptime_clock.now ()) (msg : Cstruct.t) =
    try
      let common, rest = Cstruct.split msg Message.sizeof_ts_errors in
      let number = Message.get_ts_errors_count common in
      let errors, _ = Cstruct.split rest (number * Message.sizeof_ts_error) in
      let stream =
        Stream.Multi_TS_ID.of_int32_pure @@ Message.get_ts_errors_stream_id common
      in
      let iter =
        Cstruct.iter
          (fun _ -> Some Message.sizeof_ts_error)
          (fun x ->
            let pid' = Message.get_ts_error_pid x in
            let pid = pid' land 0x1FFF in
            let err_code = Message.get_ts_error_err_code x in
            { Error.count = Message.get_ts_error_count x
            ; err_code
            ; err_ext = Message.get_ts_error_err_ext x
            ; is_t2mi = false
            ; multi_pid = pid' land 0x8000 > 0
            ; pid
            ; packet = Message.get_ts_error_packet x
            ; param_1 = Message.get_ts_error_param_1 x
            ; param_2 = Message.get_ts_error_param_2 x
            ; timestamp })
          errors
      in
      let errors =
        List.sort (fun (x : Error.t) y -> Int32.compare x.packet y.packet)
        @@ Cstruct.fold (fun acc el -> el :: acc) iter []
      in
      Ok (stream, errors)
    with Invalid_argument _ -> Error Request.Invalid_payload
end

module T2MI_error = struct
  type error =
    { code : int
    ; t2mi_stream_id : int
    ; param : int }

  let equal_error (a : error as 'a) (b : 'a) =
    a.code = b.code && a.t2mi_stream_id = b.t2mi_stream_id

  let ts_parser_code = 0xF0

  let t2mi_parser_code = 0xF1

  let get_relevant_t2mi_adv_code = function
    | 0 -> Some 2
    | 1 -> Some 3
    | 4 -> Some 4
    | 5 -> Some 5
    | 6 -> Some 6
    | 9 -> Some 7
    | 20 -> Some 8
    | _ -> None

  (* Merge t2mi errors with counter and advanced errors.
     Result is common t2mi error type *)
  let merge ~(count : error list) ~(param : error list) timestamp pid : Error.t list =
    List.map
      (fun (error : error) ->
        let param_1 =
          match get_relevant_t2mi_adv_code error.code with
          | None -> 0l
          | Some _code -> (
            (* FIXME *)
            match List.find_opt (equal_error error) param with
            | None -> 0l
            | Some x -> Int32.of_int x.param)
        in
        { Error.count = error.param
        ; err_code = error.code
        ; err_ext = 0
        ; is_t2mi = true
        ; multi_pid = false
        ; pid
        ; packet = 0l
        ; param_1
        ; param_2 = Int32.of_int error.t2mi_stream_id
        ; timestamp })
      count

  (* Map T2-MI advanced errors with 'code' = 0 to common T2-MI error type *)
  let map_t2mi_parser_errors timestamp (pid : int) =
    List.map (fun (x : error) ->
        { Error.count = 1
        ; err_code = x.code
        ; err_ext = 0
        ; is_t2mi = true
        ; multi_pid = false
        ; pid
        ; packet = 0l
        ; param_1 = Int32.of_int x.param
        ; param_2 = Int32.of_int x.t2mi_stream_id
        ; timestamp })

  (* Map ts parser flags to common T2-MI error type *)
  let map_ts_container_error timestamp (pid : int) =
    List.map (fun (x : int) ->
        { Error.count = 1
        ; err_code = ts_parser_code
        ; err_ext = 0
        ; is_t2mi = true
        ; multi_pid = false
        ; pid
        ; packet = 0l
        ; param_1 = Int32.of_int x
        ; param_2 = 0l
        ; timestamp })

  let parse_ts_container_errors (flags : int) =
    let msb = 3 in
    let rec aux acc i =
      let acc = if flags land (1 lsl i) <> 0 then i :: acc else acc in
      if i = 0 then acc else aux acc (pred i)
    in
    aux [] msb

  let parse_error (buf : Cstruct.t) =
    let index = Message.get_t2mi_error_index buf in
    let param = Message.get_t2mi_error_data buf in
    let code = index lsr 4 in
    let t2mi_stream_id = index land 7 in
    match index land 8 with
    | 0 ->
        if param > 0 (* filter zero errors *)
        then `E {code; t2mi_stream_id; param}
        else `N
    | _ ->
        if code <> 0 (* t2mi parser error *)
        then `P {code; t2mi_stream_id; param}
        else if param <> 0
        then `C {code = t2mi_parser_code; t2mi_stream_id; param}
        else `N

  let parse ?(timestamp = Ptime_clock.now ()) (msg : Cstruct.t) =
    try
      let common, rest = Cstruct.split msg Message.sizeof_t2mi_errors in
      let number = Message.get_t2mi_errors_count common in
      let errors, _ = Cstruct.split rest (number * Message.sizeof_t2mi_error) in
      let stream =
        Stream.Multi_TS_ID.of_int32_pure @@ Message.get_t2mi_errors_stream_id common
      in
      let pid = Message.get_t2mi_errors_pid common in
      (* let sync = int_to_t2mi_sync_list (get_t2mi_errors_sync common) in *)
      let iter =
        Cstruct.iter (fun _ -> Some Message.sizeof_t2mi_error) (fun x -> x) errors
      in
      let err_with_counter, err_with_param, parser_err =
        Cstruct.fold
          (fun ((cnt, adv, oth) as acc) buf ->
            match parse_error buf with
            | `N -> acc
            | `E x -> x :: cnt, adv, oth
            | `P x -> cnt, x :: adv, oth
            | `C x -> cnt, adv, x :: oth)
          iter
          ([], [], [])
      in
      let ts_container_errors =
        parse_ts_container_errors @@ Message.get_t2mi_errors_err_flags common
      in
      let errors =
        merge ~count:err_with_counter ~param:err_with_param timestamp pid
        @ map_t2mi_parser_errors timestamp pid parser_err
        @ map_ts_container_error timestamp pid ts_container_errors
      in
      Ok (stream, errors)
    with Invalid_argument _ -> Error Request.Invalid_payload
end

let check_prefix (buf : Cstruct.t) =
  try
    let prefix = Message.get_common_header_prefix buf in
    if Message.prefix <> prefix then Error (Bad_prefix prefix) else Ok buf
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_msg_code (buf : Cstruct.t) =
  try
    let code = Message.get_common_header_msg_code buf in
    let has_crc = code land 2 > 0 in
    match Request.simple_tag_of_enum @@ (code lsr 8) with
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
          (Cstruct.shift body' 2)
      in
      let crc = 0xFFFF land Cstruct.fold ( + ) iter 0 in
      let crc' = Cstruct.LE.get_uint16 crc' 0 in
      if crc <> crc'
      then Error (Bad_crc (code, crc, crc'))
      else
        let body = Cstruct.shift body' Message.sizeof_common_header in
        Ok (tag, body, rest)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let get_msg buf =
  let ( >>= ) x f =
    match x with
    | Ok x -> f x
    | Error e -> Error e
  in
  check_prefix buf >>= check_msg_code >>= check_crc

let parse_part src (buf : Cstruct.t) =
  try
    let code_ext = Message.get_complex_rsp_header_code_ext buf in
    let long = code_ext land 0x2000 <> 0 in
    let parity = code_ext land 0x1000 <> 0 in
    let param =
      Int32.mul
        2l
        (if long
        then Message.get_complex_rsp_header_ext_param buf
        else Int32.of_int @@ Message.get_complex_rsp_header_param buf)
    in
    let body =
      if long
      then Cstruct.shift buf Message.sizeof_complex_rsp_header_ext
      else Cstruct.shift buf Message.sizeof_complex_rsp_header
    in
    let id =
      { Part_id.code = code_ext land 0x0FFF
      ; client_id = Uint16.of_int @@ Message.get_complex_rsp_header_client_id buf
      ; request_id = Uint16.of_int @@ Message.get_complex_rsp_header_request_id buf }
    in
    let part = {body; first = code_ext land 0x8000 <> 0; parity; param} in
    Logs.debug ~src (fun m ->
        m
          "parser - got complex message part (code: 0x%X, req_id: %a, client_id: %a, \
           first: %B, parity: %B, length: %d, param: %ld)"
          id.code
          Uint16.pp
          id.request_id
          Uint16.pp
          id.client_id
          part.first
          parity
          (Cstruct.len body)
          param);
    Ok (id, part)
  with Invalid_argument _ -> Error "invalid payload"

let compose_parts (parts : part list) =
  let sorted = List.sort compare_part parts in
  match sorted with
  | ({first = true; _} as first) :: rest as sorted -> (
    try
      let length =
        List.fold_left
          (fun acc part ->
            if part.first = true then raise_notrace (Invalid_part Unexpected_first);
            if part.parity <> first.parity
            then raise_notrace (Invalid_part Unexpected_parity);
            if Int32.to_int part.param = acc
            then acc + Cstruct.len part.body
            else raise_notrace (Invalid_part Discontinuity))
          (Cstruct.len first.body)
          rest
      in
      if Int32.to_int first.param <> length
      then `P sorted
      else
        let acc = Cstruct.concat @@ List.map (fun x -> x.body) sorted in
        let message =
          if first.parity then fst @@ Cstruct.split acc (Cstruct.len acc - 1) else acc
        in
        `V message
    with
    | Invalid_part Discontinuity -> `P sorted
    | Invalid_part e -> `E e)
  | _ -> `P sorted

let parse ~timestamp src body parts = function
  | `Part -> (
    match parse_part src body with
    | Error e ->
        Logs.err ~src (fun m -> m "Error parsing message part: %s" e);
        None, parts
    | Ok (id, part) -> (
        let acc =
          match Part.find_opt id parts with
          | None -> {data = []; timestamp}
          | Some x -> x
        in
        match compose_parts (part :: acc.data) with
        | `P data -> None, Part.add id {acc with data} parts
        | `E e ->
            Logs.err (fun m -> m "Error composing parts: %a" pp_part_error e);
            None, Part.remove id parts
        | `V body ->
            let msg =
              match Request.complex_tag_of_enum id.code with
              | None ->
                  Logs.debug (fun m ->
                      m "Got unknown complex message (code = 0x%04X)" id.code);
                  None
              | Some tag ->
                  let msg =
                    { Request.tag
                    ; client_id = id.client_id
                    ; request_id = id.request_id
                    ; body }
                  in
                  Some {data = `Complex msg; timestamp}
            in
            msg, Part.remove id parts))
  | tag -> Some {data = `Simple {Request.tag; body}; timestamp}, parts

let deserialize ?(timestamp = Ptime_clock.now ()) src parts buf =
  let rec f ?error acc parts buf =
    if Cstruct.len buf < Message.sizeof_common_header
    then List.rev acc, parts, buf
    else
      match get_msg buf with
      | Ok (tag, body, rest) -> (
        match parse ~timestamp src body parts tag with
        | Some rsp, parts -> f (rsp :: acc) parts rest
        | None, parts -> f acc parts rest)
      | Error e -> (
        match e with
        | Insufficient_payload x -> List.rev acc, parts, x
        | e ->
            (match e, error with
            | Bad_prefix _, Some (Bad_prefix _) -> ()
            | _ -> Logs.warn ~src (fun m -> m "parser error: %s" @@ error_to_string e));
            f ~error:e acc parts (Cstruct.shift buf 1))
  in
  let responses, parts, res = f [] parts buf in
  responses, parts, if Cstruct.len res > 0 then Some res else None

let is_response (type a) (req : a Request.t) ({data; timestamp} : Request.rsp ts) :
    (a, Request.error) result option =
  match req with
  | Reset -> None
  | Get_devinfo -> (
    match data with
    | `Simple {tag = `Devinfo; body} -> Some (parse_devinfo body)
    | _ -> None)
  | Get_deverr {request_id = id; _} -> (
    match data with
    | `Complex {tag = `Deverr; body; request_id; _} when id = request_id ->
        Some (Deverr.parse ~timestamp body)
    | _ -> None)
  | Get_mode -> (
    match data with
    | `Simple {tag = `Mode; body} -> Some (parse_mode body)
    | _ -> None)
  | Set_mode _ -> None
  | Set_jitter_mode _ -> None
  | Set_src_id _ -> None
  | Get_t2mi_seq {request_id = id; _} -> (
    match data with
    | `Complex {tag = `T2mi_seq; body; request_id; _} when id = request_id ->
        Some (T2MI_sequence.parse ~timestamp body)
    | _ -> None)
  | Get_section {request_id = id; table_id; section; _} -> (
    match data with
    | `Complex {tag = `Section; body; request_id; _} when id = request_id ->
        Some (Section.parse ~table_id ~section ~timestamp body)
    | _ -> None)
  | Get_bitrate {request_id = id} -> (
    match data with
    | `Complex {tag = `Bitrate; body; request_id; _} when id = request_id ->
        Some (Bitrate.parse ~timestamp body)
    | _ -> None)
  | Get_structure {request_id = id; stream} -> (
    match data with
    | `Complex {tag = `Structure; body; request_id; _} when id = request_id ->
        Some (Structure.parse ~timestamp stream body)
    | _ -> None)
  | Get_t2mi_info {request_id = id; _} -> (
    match data with
    | `Complex {tag = `T2mi_info; body; request_id; _} when id = request_id ->
        Some (T2MI_info.parse ~timestamp body)
    | _ -> None)
