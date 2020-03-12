let name = "J2K video descriptor"

let parse bs off =
  match%bitstring bs with
  | {| profile_and_level   : 16
     ; horizontal_size     : 32 : save_offset_to (off_1)
     ; vertical_size       : 32 : save_offset_to (off_2)
     ; max_bit_rate        : 32 : save_offset_to (off_3)
     ; max_buffer_size     : 32 : save_offset_to (off_4)
     ; den_frame_rate      : 16 : save_offset_to (off_5)
     ; num_frame_rate      : 16 : save_offset_to (off_6)
     ; color_specification : 8  : save_offset_to (off_7)
     ; still_mode          : 1  : save_offset_to (off_8)
     ; interlaced_video    : 1  : save_offset_to (off_9)
     ; reserved            : 6  : save_offset_to (off_10)
     ; rest                : -1 : save_offset_to (off_11), bitstring
     |}
    ->
      let nodes =
        [
          Node.make ~offset:off 16 "profile_and_level"
            (Hex (Int profile_and_level));
          Node.make ~offset:(off + off_1) 32 "horizontal_size"
            (Dec (Int32 horizontal_size));
          Node.make ~offset:(off + off_2) 32 "vertical_size"
            (Dec (Int32 vertical_size));
          Node.make ~offset:(off + off_3) 32 "max_bit_rate"
            (Dec (Int32 max_bit_rate));
          Node.make ~offset:(off + off_4) 32 "max_buffer_size"
            (Dec (Int32 max_buffer_size));
          Node.make ~offset:(off + off_5) 16 "den_frame_rate"
            (Dec (Int den_frame_rate));
          Node.make ~offset:(off + off_6) 16 "num_frame_rate"
            (Dec (Int num_frame_rate));
          Node.make ~offset:(off + off_7) 8 "color_specification"
            (Hex (Int color_specification));
          Node.make ~offset:(off + off_8) 1 "still_mode"
            (Bits (Bool still_mode));
          Node.make ~offset:(off + off_9) 1 "interlaced_video"
            (Bits (Bool interlaced_video));
          Node.make ~offset:(off + off_10) 6 "reserved" (Bits (Int reserved));
        ]
      in
      nodes @ Bytes.parse ~offset:(off + off_11) rest "private_data_byte"
