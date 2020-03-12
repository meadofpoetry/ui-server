open Table_common

let parse =
  function%bitstring
  | {| header              : 24 : bitstring
     ; network_id          : 16 : save_offset_to (off_1)
     ; reserved            : 2  : save_offset_to (off_2)
     ; version_number      : 5  : save_offset_to (off_3)
     ; current_next_ind    : 1  : save_offset_to (off_4)
     ; section_number      : 8  : save_offset_to (off_5)
     ; last_section_number : 8  : save_offset_to (off_6)
     ; rfu_1               : 4  : save_offset_to (off_7)
     ; nw_desc_length      : 12 : save_offset_to (off_8)
     ; descriptors         : nw_desc_length * 8 : save_offset_to (off_9), bitstring
     ; rfu_2               : 4  : save_offset_to (off_10)
     ; ts_loop_len         : 12 : save_offset_to (off_11)
     ; transport_streams   : ts_loop_len * 8 : save_offset_to (off_12),bitstring
     ; crc32               : 32 : save_offset_to (off_13)
     |}
    ->
      let ts = parse_ts off_12 transport_streams in
      let dscrs = parse_descriptors off_9 descriptors in
      let header = parse_header header in
      let nodes =
        [
          Node.make ~offset:off_1 16 "network_id" (Hex (Int network_id));
          Node.make ~offset:off_2 2 "reserved" (Hex (Int reserved));
          Node.make ~offset:off_3 5 "version_number" (Dec (Int version_number));
          Node.make ~offset:off_4 1 "current_next_indicator"
            (Bits (Bool current_next_ind));
          Node.make ~offset:off_5 8 "section_number" (Dec (Int section_number));
          Node.make ~offset:off_6 8 "last_section_number"
            (Dec (Int last_section_number));
          Node.make ~offset:off_7 4 "reserved_future_use" (Hex (Int rfu_1));
          Node.make ~offset:off_8 12 "network_desc_length"
            (Hex (Int nw_desc_length));
          Node.make ~offset:off_9 (nw_desc_length * 8) "descriptors"
            (List dscrs);
          Node.make ~offset:off_10 4 "reserved_future_use" (Hex (Int rfu_2));
          Node.make ~offset:off_11 12 "transport_stream_loop_length"
            (Dec (Int ts_loop_len));
          Node.make ~offset:off_12 (ts_loop_len * 8) "transport_streams"
            (List ts);
          Node.make ~offset:off_13 32 "CRC_32" (Hex (Uint32 crc32));
        ]
      in
      header @ nodes
