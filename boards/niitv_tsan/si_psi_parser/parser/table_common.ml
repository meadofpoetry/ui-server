let parsed_length : Node.t list -> int =
  List.fold_left (fun acc (x : Node.t) -> x.length + acc) 0

let parse_header bs =
  match%bitstring bs with
  | {| table_id       : 8
     ; ssi            : 1  : save_offset_to (off_1)
     ; rfu            : 1  : save_offset_to (off_2)
     ; reserved_1     : 2  : save_offset_to (off_3)
     ; section_length : 12 : save_offset_to (off_4)
     |} ->
    [ Node.make ~offset:0     8  "table_id"        (Hex (Int table_id))
    ; Node.make ~offset:off_1 1  "section_syntax_indicator" (Bits (Bool ssi))
    ; Node.make ~offset:off_2 1  "'0'"             (Bits (Bool rfu))
    ; Node.make ~offset:off_3 2  "reserved"        (Hex (Int reserved_1))
    ; Node.make ~offset:off_4 12 "section_length"  (Dec (Int section_length))
    ]

let rec parse_descriptors = fun offset bs ->
  if Bitstring.bitstring_length bs = 0 then []
  else
    let descr,rest, off = Descriptor.parse bs offset in
    descr :: parse_descriptors off rest

let rec parse_ts = fun off x ->
  if Bitstring.bitstring_length x = 0 then []
  else
    match%bitstring x with
    | {| ts_id       : 16
       ; on_id       : 16 : save_offset_to (off_1)
       ; rfu         : 4  : save_offset_to (off_2)
       ; length      : 12 : save_offset_to (off_3)
       ; descriptors : length * 8 : save_offset_to (off_4), bitstring
       ; rest        : -1 : save_offset_to (off_5), bitstring
       |} ->
      let descriptors = parse_descriptors (off + off_4) descriptors in
      let nodes =
        [ Node.make ~offset:off           16 "transport_stream_id" (Dec (Int ts_id))
        ; Node.make ~offset:(off_1 + off) 16 "original_network_id" (Dec (Int on_id))
        ; Node.make ~offset:(off_2 + off) 4  "reserved_future_use" (Bits (Int rfu))
        ; Node.make ~offset:(off_3 + off) 12 "transport_descriptors_length" (Dec (Int length))
        ; Node.make ~offset:(off_4 + off) (length * 8) "descriptors" (List descriptors)
        ] in
      let ts_name = Printf.sprintf "transport stream %d" ts_id in
      let node = Node.make ~offset:off (parsed_length nodes) ts_name (List nodes) in
      node :: parse_ts (off_5 + off) rest
