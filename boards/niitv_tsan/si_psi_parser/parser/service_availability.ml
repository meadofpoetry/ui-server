let name = "service_availability_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| availability_flag : 1
     ; reserved          : 7  : save_offset_to (off_1)
     ; rest              : -1 : save_offset_to (off_2), bitstring
     |} ->
    let nodes =
      [ Node.make ~offset:off 1 "availability_flag" (Bits (Bool availability_flag))
      ; Node.make ~offset:(off + off_1) 7 "reserved" (Bits (Int reserved)) ]
    in
    nodes @ Bytes.parse ~bytes:2 ~offset:(off + off_2) rest "cell_id"
