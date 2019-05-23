open Table_common

let rec parse_programs = fun off x ->
  let to_name = function
    | 0 -> "network_PID"
    | _ -> "program_map_PID" in
  if Bitstring.bitstring_length x = 0 then []
  else
    match%bitstring x with
    | {| id       : 16
       ; reserved : 3  : save_offset_to (off_1)
       ; pid      : 13 : save_offset_to (off_2)
       ; rest     : -1 : save_offset_to (off_3), bitstring
       |} ->
      let nodes =
        [ Node.make ~offset:off 16 "program_number" (Hex (Int id))
        ; Node.make ~offset:(off_1 + off) 3 "reserved" (Bits (Int reserved))
        ; Node.make ~offset:(off_2 + off) 13 (to_name id) (Hex (Int pid))
        ] in
      let name = match id with
        | 0 -> "network"
        | x -> Printf.sprintf "program %d" x in
      let node = Node.make ~offset:off (parsed_length nodes) name (List nodes) in
      node :: parse_programs (off_3 + off) rest

let parse bs =
  let progs_length off = Bitstring.bitstring_length bs - off - 8 - 32 in
  match%bitstring bs with
  | {| header           : 24 : bitstring
     ; ts_id            : 16 : save_offset_to (off_1)
     ; reserved         : 2  : save_offset_to (off_2)
     ; version_number   : 5  : save_offset_to (off_3)
     ; current_next_ind : 1  : save_offset_to (off_4)
     ; section_number   : 8  : save_offset_to (off_5)
     ; last_section_num : 8  : save_offset_to (off_6)
     ; programs         : progs_length off_6 : save_offset_to (off_7), bitstring
     ; crc32            : 32 : save_offset_to (off_8)
     |} ->
    let progs  = parse_programs off_7 programs in
    let header = parse_header header in
    let nodes  =
      [ Node.make ~offset:off_1 16 "transport_stream_id" (Hex (Int ts_id))
      ; Node.make ~offset:off_2 2 "reserved" (Bits (Int reserved))
      ; Node.make ~offset:off_3 5 "version_number" (Dec (Int version_number))
      ; Node.make ~offset:off_4 1 "current_next_indicator" (Bits (Bool current_next_ind))
      ; Node.make ~offset:off_5 8 "section_number" (Dec (Int section_number))
      ; Node.make ~offset:off_6 8 "last_section_number" (Dec (Int last_section_num))
      ; Node.make ~offset:off_7 (progs_length off_6) "programs" (List progs)
      ; Node.make ~offset:off_8 32 "CRC_32" (Hex (Uint32 crc32))
      ] in
    header @ nodes
