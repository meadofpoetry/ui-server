let name = "announcement_support_descriptor"

let parse_ann_type ann =
  match ann with
  | 0 -> "Emergency alarm"
  | 1 -> "Road Traffic flash"
  | 2 -> "Public Transport flash"
  | 3 -> "Warning message"
  | 4 -> "News flash"
  | 5 -> "Weather flash"
  | 6 -> "Event announcement"
  | 7 -> "Personal call"
  | _ -> "Reserved for future use"

let parse_ref_type rf =
  match rf with
  | 0 -> "Announcement is broadcast in the usual audio stream of the service"
  | 1 -> "Announcement is broadcast in a separate audio stream that \
          is part of the service"
  | 2 -> "Announcement is broadcast by means of a different service \
          within the same transport system"
  | 3 -> "Announcement is broadcast by means of a different service \
          within a different transport stream"
  | _ -> "Reserved for future use"

let rec f off x =
  if Bitstring.bitstring_length x = 0 then []
  else match%bitstring x with
    | {| ann_type             : 4
       ; rfu                  : 1  : save_offset_to (off_1)
       ; 0x01 | 0x02 | 0x03   : 3  : save_offset_to (off_2)
       ; original_network_id  : 16 : save_offset_to (off_3)
       ; transport_stream_id  : 16 : save_offset_to (off_4)
       ; service_id           : 16 : save_offset_to (off_5)
       ; component_tag        : 8  : save_offset_to (off_6)
       ; rest                 : -1 : save_offset_to (off_7), bitstring
       |} ->
      let ann_typ = parse_ann_type ann_type in
      let ref_typ = parse_ref_type 1 in (* FIXME parsing reference type*)
      let nodes =
        [ Node.make ~parsed:ann_typ ~offset:off 4 "announcement_type" (Bits (Int ann_type))
        ; Node.make ~offset:(off + off_1) 1  "reserved_future_use" (Bits (Bool rfu))
        ; Node.make ~parsed:ref_typ ~offset:(off + off_2) 3
            "reference_type" (Hex (Int 0x01)) (* FIXME parsing reference type*)
        ; Node.make ~offset:(off + off_3) 16 "original_network_id" (Hex (Int original_network_id))
        ; Node.make ~offset:(off + off_4) 16 "transport_stream_id" (Hex (Int transport_stream_id))
        ; Node.make ~offset:(off + off_5) 16 "service_id" (Hex (Int service_id))
        ; Node.make ~offset:(off + off_6) 8  "component_tag" (Hex (Int component_tag)) ]
      in
      nodes @ f (off + off_7) rest
    | {| ann_type        : 4
       ; rfu             : 1  : save_offset_to (off_1)
       ; reference_type  : 3  : save_offset_to (off_2)
       ; rest            : -1 : save_offset_to (off_3), bitstring
       |} ->
      let ann_typ = parse_ann_type ann_type in
      let ref_typ = parse_ref_type reference_type in
      let nodes =
        [ Node.make ~parsed:ann_typ ~offset:off 4 "announcement_type" (Bits (Int ann_type))
        ; Node.make ~offset:(off + off_1) 1  "reserved_future_use" (Bits (Bool rfu))
        ; Node.make ~parsed:ref_typ ~offset:(off + off_2) 3
            "reference_type" (Hex (Int reference_type)) ]
      in
      let type_name = (Printf.sprintf "Type %s" (string_of_int ann_type)) ^ ann_typ in
      let node = Node.make ~offset:off 64 type_name (List nodes) in
      node :: f (off + off_3) rest

let parse bs off =
  match%bitstring bs with
  | {| ann_support_ind : 16
     ; rest            : -1 : save_offset_to (off_1), bitstring
     |} ->
    let node =
      Node.make ~offset:off 16 "announcement_support_indicator"
        (Hex (Int ann_support_ind)) in
    node :: f (off + off_1) rest
