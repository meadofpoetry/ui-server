let name = "NVOD_reference_descriptor"

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0 then []
  else
    match%bitstring bs with
    | {| ts_id : 16
       ; on_id : 16 : save_offset_to (off_1)
       ; sv_id : 16 : save_offset_to (off_2)
       ; rest  : -1 : save_offset_to (off_3), bitstring
       |}
      ->
        let nodes =
          [
            Node.make ~offset:off 16 "transport_stream_id" (Hex (Int ts_id));
            Node.make ~offset:(off + off_1) 16 "original_network_id"
              (Hex (Int on_id));
            Node.make ~offset:(off + off_2) 16 "service_id" (Hex (Int sv_id));
          ]
        in
        nodes @ parse rest (off + off_3)
