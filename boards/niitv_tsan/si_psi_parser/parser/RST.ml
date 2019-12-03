open Table_common

let rec parse_events off x =
  if Bitstring.bitstring_length x = 0
  then []
  else
    match%bitstring x with
    | {| ts_id      : 16
       ; on_id      : 16 : save_offset_to (off_1)
       ; service_id : 16 : save_offset_to (off_2)
       ; event_id   : 16 : save_offset_to (off_3)
       ; rfu        : 5  : save_offset_to (off_4)
       ; status     : 3  : save_offset_to (off_5)
       ; rest       : -1 : save_offset_to (off_6), bitstring
       |}
      ->
        let nodes =
          [ Node.make ~offset:off 16 "transport_stream_id" (Hex (Int ts_id))
          ; Node.make ~offset:(off_1 + off) 16 "original_network_id" (Hex (Int on_id))
          ; Node.make ~offset:(off_2 + off) 16 "service_id" (Hex (Int service_id))
          ; Node.make ~offset:(off_3 + off) 16 "event_id" (Hex (Int event_id))
          ; Node.make ~offset:(off_4 + off) 5 "reserved_future_use" (Bits (Int rfu))
          ; Node.make ~offset:(off_5 + off) 3 "running_status" (Hex (Int status)) ]
        in
        let event_name = Printf.sprintf "event %d" event_id in
        let node = Node.make ~offset:off (parsed_length nodes) event_name (List nodes) in
        node :: parse_events (off_6 + off) rest

let parse =
  function%bitstring
  | {| header : 24 : bitstring
     ; rest   : -1 : save_offset_to (off), bitstring
     |}
    ->
      let header = parse_header header in
      header @ parse_events off rest
