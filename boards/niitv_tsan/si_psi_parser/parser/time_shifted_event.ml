let name = "time_shifted_service_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| service_id : 16
     ; event_id   : 16 : save_offset_to (off_1)
     |} ->
    [ Node.make ~offset:off 16 "reference_service_id" (Hex (Int service_id))
    ; Node.make ~offset:(off + off_1) 16 "reference_event_id" (Hex (Int event_id))
    ]
