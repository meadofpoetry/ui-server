let name = "STD_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| reserved        : 7
     ; leak_valid_flag : 1 : save_offset_to (off_1)
     |}
    ->
      [
        Node.make ~offset:off 7 "reserved" (Bits (Int reserved));
        Node.make ~offset:(off_1 + off) 1 "leak_valid_flag"
          (Bits (Bool leak_valid_flag));
      ]
