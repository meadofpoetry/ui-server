let name = "maximum_bitrate_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| reserved        : 2
     ; maximum_bitrate : 22 : save_offset_to (off_1)
     |} ->
    [ Node.make ~offset:off 2 "reserved" (Bits (Int reserved))
    ; Node.make ~offset:(off+off_1) 22 "maximum_bitrate" (Dec (Uint maximum_bitrate))
    ]
