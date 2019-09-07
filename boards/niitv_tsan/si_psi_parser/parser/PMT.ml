open Table_common

let rec parse_streams off x =
  if Bitstring.bitstring_length x = 0
  then []
  else
    match%bitstring x with
    | {| stream_type    : 8
        ; reserved_1     : 3  : save_offset_to (off_1)
        ; pid            : 13 : save_offset_to (off_2)
        ; reserved_2     : 4  : save_offset_to (off_3)
        ; length         : 12 : save_offset_to (off_4)
        ; descriptors    : length * 8 : save_offset_to (off_5), bitstring
        ; rest           : -1 : save_offset_to (off_6), bitstring
        |}
      ->
        let dscrs = parse_descriptors (off + off_5) descriptors in
        let nodes =
          [ Node.make ~offset:off 8 "stream_type" (Hex (Int stream_type))
          ; Node.make ~offset:(off_1 + off) 3 "reserved" (Bits (Int reserved_1))
          ; Node.make ~offset:(off_2 + off) 13 "elementary_PID" (Hex (Int pid))
          ; Node.make ~offset:(off_3 + off) 4 "reserved" (Bits (Int reserved_2))
          ; Node.make ~offset:(off_4 + off) 12 "ES_info_length" (Hex (Int length))
          ; Node.make ~offset:(off_5 + off) (length * 8) "descriptors" (List dscrs) ]
        in
        let stream_name = Printf.sprintf "stream %d" pid in
        let node =
          Node.make ~offset:off (parsed_length nodes) stream_name (List nodes)
        in
        node :: parse_streams (off_6 + off) rest

let parse bs =
  let streams_len prog_length off =
    Bitstring.bitstring_length bs - (prog_length * 8) - off - 32
  in
  match%bitstring bs with
  | {| header           : 24 : bitstring
     ; program_number   : 16 : save_offset_to (off_1)
     ; reserved_1       : 2  : save_offset_to (off_2)
     ; version_number   : 5  : save_offset_to (off_3)
     ; current_next_ind : 1  : save_offset_to (off_4)
     ; section_number   : 8  : save_offset_to (off_5)
     ; last_section_num : 8  : save_offset_to (off_6)
     ; reserved_2       : 3  : save_offset_to (off_7)
     ; pcr_pid          : 13 : save_offset_to (off_8)
     ; reserved_3       : 4  : save_offset_to (off_9)
     ; length           : 12 : save_offset_to (off_10)
     ; descriptors      : length * 8 : bitstring, save_offset_to (off_11)
     ; streams          : streams_len length off_11 : bitstring, save_offset_to (off_12)
     ; crc32            : 32 : save_offset_to (off_13)
     |}
    ->
      let dscrs = parse_descriptors off_11 descriptors in
      let header = parse_header header in
      let streams = parse_streams off_12 streams in
      let nodes =
        [ Node.make ~offset:off_1 16 "program_number" (Dec (Int program_number))
        ; Node.make ~offset:off_2 2 "reserved" (Bits (Int reserved_1))
        ; Node.make ~offset:off_3 5 "version_number" (Dec (Int version_number))
        ; Node.make
            ~offset:off_4
            1
            "current_next_indicator"
            (Bits (Bool current_next_ind))
        ; Node.make ~offset:off_5 8 "section_number" (Dec (Int section_number))
        ; Node.make ~offset:off_6 8 "last_section_number" (Dec (Int last_section_num))
        ; Node.make ~offset:off_7 3 "reserved" (Bits (Int reserved_2))
        ; Node.make ~offset:off_8 13 "PCR_PID" (Bits (Int pcr_pid))
        ; Node.make ~offset:off_9 4 "reserved" (Bits (Int reserved_3))
        ; Node.make ~offset:off_10 12 "program_info_length" (Dec (Int length))
        ; Node.make ~offset:off_11 (length * 8) "descriptors" (List dscrs)
        ; Node.make ~offset:off_12 (streams_len length off_11) "streams" (List streams)
        ; Node.make ~offset:off_13 32 "CRC_32" (Hex (Uint32 crc32)) ]
      in
      header @ nodes
