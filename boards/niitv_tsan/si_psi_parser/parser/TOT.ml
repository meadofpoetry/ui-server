open Table_common

let parse =
  function%bitstring
  | {| header      : 24 : bitstring
     ; utc_time    : 40 : save_offset_to (off_1), bitstring
     ; reserved    : 4  : save_offset_to (off_2)
     ; length      : 12 : save_offset_to (off_3)
     ; descriptors : length * 8 : save_offset_to (off_4), bitstring
     ; crc32       : 32 : save_offset_to (off_5)
     |}
    ->
      let dscrs = parse_descriptors off_4 descriptors in
      let header = parse_header header in
      let utc_time =
        match Date_time.parse_timestamp utc_time with
        | Some x -> Node.Time x
        | None -> (
            match%bitstring utc_time with
            | {| i : 40 |} -> Node.Dec (Uint64 i))
      in
      let nodes =
        [ Node.make ~offset:off_1 40 "utc_time" utc_time
        ; Node.make ~offset:off_2 4 "reserved" (Hex (Int reserved))
        ; Node.make ~offset:off_3 12 "descriptors_loop_length" (Hex (Int length))
        ; Node.make ~offset:off_4 (length * 8) "descriptors" (List dscrs)
        ; Node.make ~offset:off_5 32 "CRC_32" (Hex (Uint32 crc32)) ]
      in
      header @ nodes
