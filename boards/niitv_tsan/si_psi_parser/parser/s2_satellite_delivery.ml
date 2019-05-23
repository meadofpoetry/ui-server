let name = "S2_satellite_delivery_system_descriptor"

let f ~sss ~misf off x =
  match sss, misf with
  | true, true -> (match%bitstring x with
        {| reserved : 6
         ; ssi      : 18 : save_offset_to (off_1)
         ; isi      : 8  : save_offset_to (off_2)
         |} ->
        [ Node.make ~offset:off 6 "Reserved" (Bits (Int reserved))
        ; Node.make ~offset:(off + off_1) 18 "scrambling_sequence_index" (Dec (Int ssi))
        ; Node.make ~offset:(off + off_2) 8 "input_stream_identifier" (Dec (Int isi)) ])
  | true, false -> (match%bitstring x with
        {| reserved : 6
         ; ssi      : 18 : save_offset_to (off_1)
         |} ->
        [ Node.make ~offset:off 6 "Reserved" (Bits (Int reserved))
        ; Node.make ~offset:(off + off_1) 18 "scrambling_sequence_index" (Dec (Int ssi))])
  | false, true -> (match%bitstring x with
        {| isi : 8 |} ->
        [ Node.make ~offset:off 8 "input_stream_identifier" (Dec (Int isi)) ])
  | false, false -> []


let parse bs off =
  match%bitstring bs with
  | {| sss       : 1
     ; misf      : 1  : save_offset_to (off_1)
     ; backw_ind : 1  : save_offset_to (off_2)
     ; rfu       : 5  : save_offset_to (off_3)
     ; rest      : -1 : save_offset_to (off_4), bitstring
     |} ->
    let nodes =
      [ Node.make ~offset:off 1 "scrambling_sequence_selector" (Bits (Bool sss))
      ; Node.make ~offset:(off + off_1) 1 "multiple_input_stream_flag" (Bits (Bool misf))
      ; Node.make ~offset:(off + off_2) 1 "backwards_compatibility_indicator" (Bits (Bool backw_ind))
      ; Node.make ~offset:(off + off_3) 5 "reserved_future_use" (Bits (Int rfu))
      ]
    in
    nodes @ (f ~sss ~misf (off + off_4) rest)
