open Table_common

let parse_status = function
  | 0 -> "undefined"
  | 1 -> "not running"
  | 2 -> "starts in a few seconds"
  | 3 -> "pausing"
  | 4 -> "running"
  | 5 -> "service off-air"
  | 6 | 7 -> "reserved for future use"
  | _ -> "status error"

let rec parse_services off x =
  if Bitstring.bitstring_length x = 0 then []
  else
    match%bitstring x with
    | {| service_id     : 16
        ; rfu            : 6  : save_offset_to (off_1)
        ; schedule_flag  : 1  : save_offset_to (off_2)
        ; present_flag   : 1  : save_offset_to (off_3)
        ; running_status : 3  : save_offset_to (off_4)
        ; free_ca_mode   : 1  : save_offset_to (off_5)
        ; length         : 12 : save_offset_to (off_6)
        ; descriptors    : length * 8 : save_offset_to (off_7), bitstring
        ; rest           : -1 : save_offset_to (off_8), bitstring
        |}
      ->
        let dscrs = parse_descriptors (off + off_7) descriptors in
        let nodes =
          [
            Node.make ~offset:off 16 "service_id" (Hex (Int service_id));
            Node.make ~offset:(off_1 + off) 6 "reserved_fuure_use"
              (Bits (Int rfu));
            Node.make ~offset:(off_2 + off) 1 "EIT_schedule_flag"
              (Bits (Bool schedule_flag));
            Node.make ~offset:(off_3 + off) 1 "EIT_present_following_flag"
              (Bits (Bool present_flag));
            Node.make ~offset:(off_4 + off) 3 "runnning_status"
              (Hex (Int running_status));
            Node.make ~offset:(off_5 + off) 1 "free_CA_mode"
              (Bits (Bool free_ca_mode));
            Node.make ~offset:(off_6 + off) 12 "descriptors_loop_length"
              (Hex (Int length));
            Node.make ~offset:(off_7 + off) (length * 8) "descriptors"
              (List dscrs);
          ]
        in
        let parsed = parse_status running_status in
        let service_name = Printf.sprintf "service %d" service_id in
        let node =
          Node.make ~parsed ~offset:off (parsed_length nodes) service_name
            (List nodes)
        in
        node :: parse_services (off_8 + off) rest

let parse bs =
  let services_length off = Bitstring.bitstring_length bs - off - 8 - 32 in
  match%bitstring bs with
  | {| header           : 24 : bitstring
     ; ts_id            : 16 : save_offset_to (off_1)
     ; reserved         : 2  : save_offset_to (off_2)
     ; version_number   : 5  : save_offset_to (off_3)
     ; current_next_ind : 1  : save_offset_to (off_4)
     ; section_number   : 8  : save_offset_to (off_5)
     ; last_section_num : 8  : save_offset_to (off_6)
     ; on_id            : 16 : save_offset_to (off_7)
     ; rfu              : 8  : save_offset_to (off_8)
     ; services         : services_length off_8 : save_offset_to (off_9), bitstring
     ; crc32            : 32 : save_offset_to (off_10)
     |}
    ->
      let services = parse_services off_9 services in
      let header = parse_header header in
      let nodes =
        [
          Node.make ~offset:off_1 16 "transport_stream_id" (Hex (Int ts_id));
          Node.make ~offset:off_2 2 "reserved" (Hex (Int reserved));
          Node.make ~offset:off_3 5 "version_number" (Dec (Int version_number));
          Node.make ~offset:off_4 1 "current_next_indicator"
            (Bits (Bool current_next_ind));
          Node.make ~offset:off_5 8 "section_number" (Dec (Int section_number));
          Node.make ~offset:off_6 8 "last_section_number"
            (Dec (Int last_section_num));
          Node.make ~offset:off_7 16 "original_network_id" (Hex (Int on_id));
          Node.make ~offset:off_8 8 "reserved_future_use" (Hex (Int rfu));
          Node.make ~offset:off_9 (services_length off_8) "services"
            (List services);
          Node.make ~offset:off_10 32 "CRC_32" (Hex (Uint32 crc32));
        ]
      in
      header @ nodes
