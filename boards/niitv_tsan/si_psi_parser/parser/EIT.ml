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

let rec parse_events = fun off x ->
  if Bitstring.bitstring_length x = 0 then []
  else
    (match%bitstring x with
     | {| event_id       : 16
        ; start_time     : 40 : save_offset_to (off_1), bitstring
        ; duration       : 24 : save_offset_to (off_2), bitstring
        ; running_status : 3  : save_offset_to (off_3)
        ; free_ca_mode   : 1  : save_offset_to (off_4)
        ; length         : 12 : save_offset_to (off_5)
        ; descriptors    : length * 8 : save_offset_to (off_6), bitstring
        ; rest           : -1 : save_offset_to (off_7), bitstring
        |} ->
       let dscrs = parse_descriptors (off + off_6) descriptors in
       let time = match Date_time.parse_timestamp start_time with
         | Some x -> Node.Time x
         | None -> match%bitstring start_time with
           | {| i : 40 |} -> Node.Dec (Uint64  i) in
       let dur  = match Date_time.parse_duration duration with
         | Some x -> Node.Duration x
         | None -> match%bitstring duration with
           | {| i : 24 |} -> Node.Dec (Uint i) in
       let parsed = parse_status running_status in
       let nodes =
         [ Node.make ~offset:off           16 "event_id" (Hex (Int event_id))
         ; Node.make ~offset:(off_1 + off) 40 "start_time" time
         ; Node.make ~offset:(off_2 + off) 24 "duration" dur
         ; Node.make ~offset:(off_3 + off) 3  "running_status" (Dec (Int running_status))
         ; Node.make ~offset:(off_4 + off) 1  "free_CA_mode" (Bits (Bool free_ca_mode))
         ; Node.make ~offset:(off_5 + off) 12 "decriptors_loop_length" (Dec (Int length))
         ; Node.make ~offset:(off_6 + off) (length * 8) "descriptors" (List dscrs)
         ]
       in
       let event_name = Printf.sprintf "event %d" event_id in
       let node = Node.make ~parsed ~offset:off (parsed_length nodes) event_name (List nodes) in
       node :: parse_events (off_7 + off) rest)

let parse bs =
  let events_length off = Bitstring.bitstring_length bs - off - 8 - 32 in
  match%bitstring bs with
  | {| header           : 24 : bitstring
     ; service_id       : 16 : save_offset_to (off_1)
     ; reserved         : 2  : save_offset_to (off_2)
     ; version_number   : 5  : save_offset_to (off_3)
     ; current_next_ind : 1  : save_offset_to (off_4)
     ; section_number   : 8  : save_offset_to (off_5)
     ; last_section_num : 8  : save_offset_to (off_6)
     ; ts_id            : 16 : save_offset_to (off_7)
     ; on_id            : 16 : save_offset_to (off_8)
     ; seg_last_sec_num : 8  : save_offset_to (off_9)
     ; last_table_id    : 8  : save_offset_to (off_10)
     ; events           : events_length off_10 : save_offset_to (off_11), bitstring
     ; crc32            : 32 : save_offset_to (off_12)
     |} ->
    let events = parse_events off_11 events in
    let header = parse_header header in
    let nodes =
      [ Node.make ~offset:off_1 16 "service_id" (Hex (Int service_id))
      ; Node.make ~offset:off_2 2 "reserved" (Hex (Int reserved))
      ; Node.make ~offset:off_3 5 "version_number" (Dec (Int version_number))
      ; Node.make ~offset:off_4 1 "current_next_indicator" (Bits (Bool current_next_ind))
      ; Node.make ~offset:off_5 8 "section_number" (Dec (Int section_number))
      ; Node.make ~offset:off_6 8 "last_section_number" (Dec (Int last_section_num))
      ; Node.make ~offset:off_7 16 "transport_stream_id" (Hex (Int ts_id))
      ; Node.make ~offset:off_8 16 "original_network_id" (Hex (Int on_id))
      ; Node.make ~offset:off_9 8 "segment_last_section_number" (Dec (Int seg_last_sec_num))
      ; Node.make ~offset:off_10 8 "last_table_id" (Hex (Int last_table_id))
      ; Node.make ~offset:off_11 (events_length off_10) "events" (List events)
      ; Node.make ~offset:off_12 32 "CRC_32" (Hex (Uint32 crc32))
      ]
    in
    header @ nodes
