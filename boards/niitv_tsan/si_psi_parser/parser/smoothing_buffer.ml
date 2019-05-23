let name = "smoothing_buffer_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| reserved_1   : 2
     ; sb_leak_rate : 22 : save_offset_to (off_1)
     ; reserved_2   : 2  : save_offset_to (off_2)
     ; sb_size      : 22 : save_offset_to (off_3)
     |} ->
    [ Node.make ~offset:off 2 "reserved" (Bits (Int reserved_1))
    ; Node.make ~offset:(off_1 + off) 22 "sb_leak_rate" (Dec (Int sb_leak_rate))
    ; Node.make ~offset:(off_2 + off) 2  "reserved" (Bits (Int reserved_2))
    ; Node.make ~offset:(off_3 + off) 22 "sb_size" (Dec (Int sb_size))
    ]
