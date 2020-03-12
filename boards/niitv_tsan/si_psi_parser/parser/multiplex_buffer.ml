let name = "multiplexBuffer_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| mb_buffer_size : 24
     ; tb_leak_rate   : 24 : save_offset_to (off_1)
     |}
    ->
      [
        Node.make ~offset:off 24 "MB_buffer_size" (Hex (Int mb_buffer_size));
        Node.make ~offset:(off + off_1) 24 "TB_leak_rate"
          (Hex (Int tb_leak_rate));
      ]
