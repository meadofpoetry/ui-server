let name = "metadata_STD_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| reserved_1   : 2
     ; metadata_in  : 22 : save_offset_to (off_1)
     ; reserved_2   : 2  : save_offset_to (off_2)
     ; metadata_buf : 22 : save_offset_to (off_3)
     ; reserved_3   : 2  : save_offset_to (off_4)
     ; metadata_out : 22 : save_offset_to (off_5)
     |}
    ->
      [
        Node.make ~offset:off 2 "reserved" (Bits (Int reserved_1));
        Node.make ~offset:(off + off_1) 22 "metadata_input_leak_rate"
          (Dec (Int metadata_in));
        Node.make ~offset:(off + off_2) 2 "reserved" (Bits (Int reserved_2));
        Node.make ~offset:(off + off_3) 22 "metadata_buffer_size"
          (Dec (Int metadata_buf));
        Node.make ~offset:(off + off_4) 2 "reserved" (Bits (Int reserved_3));
        Node.make ~offset:(off + off_5) 22 "metadata_output_leak_rate"
          (Dec (Int metadata_out));
      ]
