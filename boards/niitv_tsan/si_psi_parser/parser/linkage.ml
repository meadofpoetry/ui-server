let name = "linkage_descriptor"

let parse_linkage_type = function
  | 0x00 | 0xFF -> "reserved for future use"
  | 0x01 -> "information service"
  | 0x02 -> "EPG service"
  | 0x03 -> "CA replacement service"
  | 0x04 -> "TS containing complete Network/Bouquet SI"
  | 0x05 -> "service replacement service"
  | 0x06 -> "data broadcast service"
  | 0x07 -> "RCS Map"
  | 0x08 -> "mobile hand-over"
  | 0x09 -> "System Software Update Service (ERSI TS 102 006 [11])"
  | 0x0A -> "TS containing SSU BAT or NIT (ETSI EN 102 006 [11])"
  | 0x0B -> "IP/MAC Notification Service (ETSI EN 301 192 [4])"
  | 0x0C -> "TS containing INT BAT or NIT (ETSI EN 301 192 [4])"
  | 0x0D -> "event linkage"
  | x when x >= 0x0E && x <= 0x1F -> "extended event linkage"
  | x when x >= 0x20 && x <= 0x7F -> "reserved for future use"
  | x when x >= 0x80 && x <= 0xFE -> "user defined"
  | _ -> assert false

let parse_handover_type = function
  | 0x00 -> "reserved for future use"
  | 0x01 -> "DVB hand-over to and identical service in a neighbouring country"
  | 0x02 -> "DVB hand-over to an associated service"
  | 0x03 -> "DVB hand-over to an associated service"
  | x when x >= 0x04 && x <= 0x0F -> "reserved for future use"
  | _ -> assert false

let parse_origin_type = function
  | 0x00 -> "NIT"
  | 0x01 -> "SDT"
  | _ -> assert false

let parse_link_type ~linkage ~link =
  match linkage with
  | 0x0E -> (
    match link with
    | 0 -> "SD"
    | 1 -> "HD"
    | 2 -> "frame compatible plano-stereoscopic H.264/AVC"
    | 3 -> "service compatible plano-stereoscopic MVC"
    | _ -> assert false)
  | 0x0F -> (
    match link with
    | 0 -> "UHD"
    | 1 -> "service frame compatible plano-stereoscopic"
    | 2 | 3 -> "reserved or future use"
    | _ -> assert false)
  | x when x >= 0x10 && x <= 0x1F -> (
    match link with
    | x when x >= 0 && x <= 3 -> "reserved for future use"
    | _ -> assert false)
  | _ -> assert false

let parse_target_id = function
  | 0 -> "use transport_stream_id"
  | 1 -> "use target_transport_stream_id"
  | 2 -> "match any transport_stream_id (wildcard)"
  | 3 -> "use user_defined_id"
  | _ -> assert false

let decode_handover_linkage bs off =
  match%bitstring bs with
  | {| 0x01        : 4
     ; rfu         : 3  : save_offset_to (off_1)
     ; origin_type : 1  : save_offset_to (off_2)
     ; network_id  : 16 : save_offset_to (off_3)
     |}
    ->
      [ Node.make ~offset:off 4 "hand-over_type" (Hex (Int 0x01))
      ; Node.make ~offset:(off + off_1) 3 "reserved_future_use" (Bits (Int rfu))
      ; Node.make ~offset:(off + off_2) 1 "origin_type" (Bits (Bool origin_type))
      ; Node.make ~offset:(off + off_3) 16 "network_id" (Dec (Int network_id)) ]
  | {| 0x02        : 4
     ; rfu         : 3  : save_offset_to (off_1)
     ; origin_type : 1  : save_offset_to (off_2)
     ; network_id  : 16 : save_offset_to (off_3)
     |}
    ->
      [ Node.make ~offset:off 4 "hand-over_type" (Hex (Int 0x02))
      ; Node.make ~offset:(off + off_1) 3 "reserved_future_use" (Bits (Int rfu))
      ; Node.make ~offset:(off + off_2) 1 "origin_type" (Bits (Bool origin_type))
      ; Node.make ~offset:(off + off_3) 16 "network_id" (Dec (Int network_id)) ]
  | {| 0x03        : 4
     ; rfu         : 3  : save_offset_to (off_1)
     ; origin_type : 1  : save_offset_to (off_2)
     ; network_id  : 16 : save_offset_to (off_3)
     |}
    ->
      [ Node.make ~offset:off 4 "hand-over_type" (Hex (Int 0x03))
      ; Node.make ~offset:(off + off_1) 3 "reserved_future_use" (Bits (Int rfu))
      ; Node.make ~offset:(off + off_2) 1 "origin_type" (Bits (Bool origin_type))
      ; Node.make ~offset:(off + off_3) 16 "network_id" (Dec (Int network_id)) ]
  | {| 0x00            : 4
     ; rfu             : 3  : save_offset_to (off_1)
     ; origin_type     : 1  : save_offset_to (off_2)
     ; init_service_id : 16 : save_offset_to (off_3)
     |}
    ->
      [ Node.make ~offset:off 4 "hand-over_type" (Hex (Int 0x03))
      ; Node.make ~offset:(off + off_1) 3 "reserved_future_use" (Bits (Int rfu))
      ; Node.make ~offset:(off + off_2) 1 "origin_type" (Bits (Bool origin_type))
      ; Node.make
          ~offset:(off + off_3)
          16
          "initial_service_id"
          (Dec (Int init_service_id)) ]

let decode_event_linkage bs off =
  match%bitstring bs with
  | {| target_event_id : 16
     ; target_listed   : 1 : save_offset_to (off_1)
     ; event_simulcast : 1 : save_offset_to (off_2)
     ; reserved        : 6 : save_offset_to (off_3)
     |}
    ->
      [ Node.make ~offset:off 16 "target_event_id" (Dec (Int target_event_id))
      ; Node.make ~offset:(off + off_1) 1 "target_listed" (Bits (Bool target_listed))
      ; Node.make ~offset:(off + off_2) 1 "event_simulcast" (Bits (Bool event_simulcast))
      ; Node.make ~offset:(off + off_3) 6 "reserved" (Bits (Int reserved)) ]

let decode_extended_event_linkage ~linkage bs off =
  let rec f off x =
    if Bitstring.bitstring_length x = 0
    then []
    else
      match%bitstring x with
      | {| target_event_id : 16
         ; target_listed   : 1  : save_offset_to (off_1)
         ; event_simulcast : 1  : save_offset_to (off_2)
         ; link_type       : 2  : save_offset_to (off_3)
         ; target_id_type  : 2  : save_offset_to (off_4)
         ; on_id_flag      : 1  : save_offset_to (off_5)
         ; service_id_flag : 1  : save_offset_to (off_6)
         ; rest            : -1 : save_offset_to (off_7), bitstring
         |}
        ->
          let nodes =
            [ Node.make ~offset:off 16 "target_event_id" (Dec (Int target_event_id))
            ; Node.make
                ~offset:(off + off_1)
                1
                "target_listed"
                (Bits (Bool target_listed))
            ; Node.make
                ~offset:(off + off_2)
                1
                "event_simulcast"
                (Bits (Bool event_simulcast))
            ; Node.make
                ~parsed:(parse_link_type ~linkage ~link:link_type)
                ~offset:(off + off_3)
                2
                "link_type"
                (Hex (Int link_type))
            ; Node.make
                ~parsed:(parse_target_id target_id_type)
                ~offset:(off + off_4)
                2
                "target_id_type"
                (Hex (Int target_id_type))
            ; Node.make
                ~offset:(off + off_5)
                1
                "original_network_id_flag"
                (Bits (Bool on_id_flag))
            ; Node.make
                ~offset:(off + off_6)
                1
                "service_id_flag"
                (Bits (Bool service_id_flag)) ]
          in
          nodes @ f (off + off_7) rest
  in
  match%bitstring bs with
  | {| loop_length    : 8
     ; extended_event : loop_length * 8 : save_offset_to (off_1), bitstring
     ; rest           : -1 : save_offset_to (off_2), bitstring
     |}
    ->
      let extended_linkage = f (off + off_1) extended_event in
      let private_data = Bytes.parse ~offset:(off + off_2) rest "private_data_byte" in
      [ Node.make ~offset:off 8 "loop_length" (Dec (Int loop_length))
      ; Node.make
          ~offset:(off + off_1)
          (loop_length * 8)
          "extended_event_linkage"
          (List extended_linkage)
      ; Node.make
          ~offset:(off + off_1)
          (Bitstring.bitstring_length rest)
          "private_data_bytes"
          (List private_data) ]

let parse bs off =
  match%bitstring bs with
  | {| ts_id                : 16
     ; on_id                : 16 : save_offset_to (off_1)
     ; service_id           : 16 : save_offset_to (off_2)
     ; 0x08                 : 8  : save_offset_to (off_3)
     ; mobile_handover_info : 24 : save_offset_to (off_4), bitstring
     ; rest                 : -1 : save_offset_to (off_5), bitstring
     |}
    ->
      let handover = decode_handover_linkage mobile_handover_info (off + off_4) in
      let bytes = Bytes.parse ~offset:(off + off_5) rest "private_data_bytes" in
      [ Node.make ~offset:off 16 "transport_stream_id" (Hex (Int ts_id))
      ; Node.make ~offset:(off + off_1) 16 "original_network_id" (Hex (Int on_id))
      ; Node.make ~offset:(off + off_2) 16 "service_id" (Hex (Int service_id))
      ; Node.make ~offset:(off + off_3) 8 "linkage_type" (Dec (Int 0x08))
      ; Node.make ~offset:(off + off_4) 24 "mobile_handover_info" (List handover)
      ; Node.make
          ~offset:(off + off_5)
          (Bitstring.bitstring_length rest)
          "private_data_bytes"
          (List bytes) ]
  | {| ts_id              : 16
     ; on_id              : 16 : save_offset_to (off_1)
     ; service_id         : 16 : save_offset_to (off_2)
     ; 0x0D               : 8  : save_offset_to (off_3)
     ; event_linkage_info : 24 : save_offset_to (off_4), bitstring
     ; rest               : -1 : save_offset_to (off_5), bitstring
     |}
    ->
      let bytes = Bytes.parse ~offset:(off + off_5) rest "private_data_bytes" in
      let event_linkage = decode_event_linkage event_linkage_info (off + off_4) in
      [ Node.make ~offset:off 16 "transport_stream_id" (Hex (Int ts_id))
      ; Node.make ~offset:(off + off_1) 16 "original_network_id" (Hex (Int on_id))
      ; Node.make ~offset:(off + off_2) 16 "service_id" (Hex (Int service_id))
      ; Node.make ~offset:(off + off_3) 8 "linkage_type" (Hex (Int 0x0D))
      ; Node.make ~offset:(off + off_4) 24 "event_linkage_info" (List event_linkage)
      ; Node.make
          ~offset:(off + off_5)
          (Bitstring.bitstring_length rest)
          "private_data_bytes"
          (List bytes) ]
  | {| ts_id        : 16
     ; on_id        : 16 : save_offset_to (off_1)
     ; service_id   : 16 : save_offset_to (off_2)
     ; linkage_type : 8  : save_offset_to (off_3)
     ; rest         : -1 : save_offset_to (off_4), bitstring
     |}
    ->
      let bytes = Bytes.parse ~offset:(off + off_4) rest "private_data_bytes" in
      [ Node.make ~offset:off 16 "transport_stream_id" (Hex (Int ts_id))
      ; Node.make ~offset:(off + off_1) 16 "original_network_id" (Hex (Int on_id))
      ; Node.make ~offset:(off + off_2) 16 "service_id" (Hex (Int service_id))
      ; Node.make
          ~parsed:(parse_linkage_type linkage_type)
          ~offset:(off + off_3)
          8
          "linkage_type"
          (Hex (Int linkage_type))
      ; Node.make
          ~offset:(off + off_4)
          (Bitstring.bitstring_length rest)
          "private_data_bytes"
          (List bytes) ]
