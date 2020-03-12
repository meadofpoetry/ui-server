let name = "Stereoscopic_program_info_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| reserved     : 5
     ; service_type : 3 : save_offset_to (off_1)
     |}
    ->
      [
        Node.make ~offset:off 5 "reserved" (Bits (Int reserved));
        Node.make ~offset:(off + off_1) 3 "stereoscopic_service_type"
          (Bits (Int service_type));
      ]
