open Table_common

let parse bs =
  let dscrs_length off = Bitstring.bitstring_length bs - off - 8 -32 in
  match%bitstring bs with
  | {| header              : 24 : bitstring
     ; reserved            : 18 : save_offset_to (off_1)
     ; version_number      : 5  : save_offset_to (off_2)
     ; current_next_ind    : 1  : save_offset_to (off_3)
     ; section_number      : 8  : save_offset_to (off_4)
     ; last_section_number : 8  : save_offset_to (off_5)
     ; descriptors         : dscrs_length off_5 : save_offset_to (off_6), bitstring
     ; crc32               : 32 : save_offset_to (off_7)
     |} ->
    let dscrs  = parse_descriptors off_6 descriptors in
    let header = parse_header header in
    let nodes  =
      [ Node.make ~offset:off_1 18 "reserved" (Bits (Int reserved))
      ; Node.make ~offset:off_2 5 "version_number" (Dec (Int version_number))
      ; Node.make ~offset:off_3 1 "current_next_indicator" (Bits (Bool current_next_ind))
      ; Node.make ~offset:off_4 8 "section_number" (Dec (Int section_number))
      ; Node.make ~offset:off_5 8 "last_section_number" (Dec (Int last_section_number))
      ; Node.make ~offset:off_6 (dscrs_length off_5) "descriptors" (List dscrs)
      ; Node.make ~offset:off_7 32 "CRC_32" (Dec (Uint32 crc32))
      ]
    in
    header @ nodes
