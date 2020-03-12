let name = "service_move_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| new_on_id : 16
     ; new_ts_id : 16 : save_offset_to (off_1)
     ; new_sv_id : 16 : save_offset_to (off_2)
     |}
    ->
      [
        Node.make ~offset:off 16 "new_original_network_id" (Hex (Int new_on_id));
        Node.make ~offset:(off + off_1) 16 "new_transport_stream_id"
          (Hex (Int new_ts_id));
        Node.make ~offset:(off + off_2) 16 "new_service_id"
          (Hex (Int new_sv_id));
      ]
