let name = "multiplex_buffer_utilization_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| bound_valid_flag : 1
     ; lower_bound      : 15 : save_offset_to (off_1)
     ; reserved         : 1  : save_offset_to (off_2)
     ; upper_bound      : 15 : save_offset_to (off_3)
     |}
    ->
      [ Node.make ~offset:off 1 "bound_valid_flag" (Bits (Bool bound_valid_flag))
      ; Node.make
          ~offset:(off_1 + off)
          15
          "LTW_offset_lower_bound"
          (Dec (Int lower_bound))
      ; Node.make ~offset:(off_2 + off) 1 "reserved" (Bits (Bool reserved))
      ; Node.make
          ~offset:(off_3 + off)
          15
          "LTW_offset_upper_bound"
          (Dec (Int upper_bound)) ]
