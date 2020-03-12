open Table_common

let rec parse_services off x =
  if Bitstring.bitstring_length x = 0 then []
  else
    match%bitstring x with
    | {| service_id     : 16
        ; dvb_rfu        : 1  : save_offset_to (off_1)
        ; running_status : 3  : save_offset_to (off_2)
        ; length         : 12 : save_offset_to (off_3)
        ; descriptors    : length * 8 : save_offset_to (off_4), bitstring
        ; rest           : -1 : save_offset_to (off_5), bitstring
        |}
      ->
        let dscrs = parse_descriptors (off + off_4) descriptors in
        let nodes =
          [
            Node.make ~offset:off 16 "service_id" (Hex (Int service_id));
            Node.make ~offset:(off_1 + off) 1 "reserved_future_use"
              (Bits (Bool dvb_rfu));
            Node.make ~offset:(off_2 + off) 3 "running_status"
              (Dec (Int running_status));
            Node.make ~offset:(off_3 + off) 12 "service_loop_length"
              (Dec (Int length));
            Node.make ~offset:(off_4 + off) (length * 8) "descriptors"
              (List dscrs);
          ]
        in
        let service_name = Printf.sprintf "event %d" service_id in
        let node =
          Node.make ~offset:off (parsed_length nodes) service_name (List nodes)
        in
        node :: parse_services (off_5 + off) rest

let parse bs =
  let services_length off till =
    Bitstring.bitstring_length bs - off - (till * 8) - 32
  in
  match%bitstring bs with
  | {| header           : 24 : bitstring
     ; dvb_rfu_1        : 16 : save_offset_to (off_1)
     ; iso_reserved     : 2  : save_offset_to (off_2)
     ; version_number   : 5  : save_offset_to (off_3)
     ; current_next_ind : 1  : save_offset_to (off_4)
     ; section_number   : 8  : save_offset_to (off_5)
     ; last_section_num : 8  : save_offset_to (off_6)
     ; dvb_rfu_2        : 4  : save_offset_to (off_7)
     ; len              : 12 : save_offset_to (off_8)
     ; descriptors      : len * 8 : bitstring, save_offset_to (off_9)
     ; services         : services_length off_9 len : save_offset_to (off_10), bitstring
     ; crc32            : 32 : save_offset_to (off_11)
     ; rest             : -1 : bitstring
     |}
    when Bitstring.bitstring_length rest = 0 ->
      let services = parse_services off_10 services in
      let dscrs = parse_descriptors off_9 descriptors in
      let nodes =
        [
          Node.make ~offset:off_1 16 "dvb_rfu_1" (Bits (Int dvb_rfu_1));
          Node.make ~offset:off_2 2 "ISO_reserved" (Bits (Int iso_reserved));
          Node.make ~offset:off_3 5 "version_number" (Dec (Int version_number));
          Node.make ~offset:off_4 1 "current_next_indicator"
            (Bits (Bool current_next_ind));
          Node.make ~offset:off_5 8 "section_number" (Dec (Int section_number));
          Node.make ~offset:off_6 8 "last_section_number"
            (Dec (Int last_section_num));
          Node.make ~offset:off_7 4 "dvb_rfu_2" (Bits (Int dvb_rfu_2));
          Node.make ~offset:off_8 12 "transmission_info_loop_length"
            (Dec (Int len));
          Node.make ~offset:off_9 (len * 8) "descriptors" (List dscrs);
          Node.make ~offset:off_10
            (services_length off_9 len)
            "services" (List services);
          Node.make ~offset:off_11 32 "CRC_32" (Hex (Uint32 crc32));
        ]
      in
      let header = parse_header header in
      header @ nodes
