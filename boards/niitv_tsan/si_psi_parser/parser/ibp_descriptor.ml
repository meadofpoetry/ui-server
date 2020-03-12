let name = "IBP_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| closed_gop_flag    : 1
     ; identical_gop_flag : 1  : save_offset_to (off_1)
     ; max_gop_length     : 14 : save_offset_to (off_2)
     |}
    ->
      [
        Node.make ~offset:off 7 "closed_gop_flag" (Bits (Bool closed_gop_flag));
        Node.make ~offset:(off_1 + off) 1 "identical_gop_flag"
          (Bits (Bool identical_gop_flag));
        Node.make ~offset:(off_2 + off) 1 "max_gop_length"
          (Dec (Int max_gop_length));
      ]
