let name = "video_stream_descriptor"

let frame_rate_to_string = function
  | 0b0000 -> "Forbidden"
  | 0b0001 -> "23.976"
  | 0b0010 -> "24"
  | 0b0011 -> "25"
  | 0b0100 -> "29.97"
  | 0b0101 -> "30"
  | 0b0110 -> "50"
  | 0b0111 -> "59.94"
  | 0b1000 -> "60"
  | x -> Value.rfu_to_string x

let chroma_format_to_string = function
  | 0b01 -> "4:2:0"
  | 0b10 -> "4:2:2"
  | 0b11 -> "4:4:4"
  | x -> Value.rfu_to_string x

let parse (bs : Bitstring.t) off =
  match%bitstring bs with
  | {| mfr_flag        : 1
     ; frame_rate      : 4 : save_offset_to (off_1)
     ; true            : 1 : save_offset_to (off_2)
     ; const_par_flag  : 1 : save_offset_to (off_3)
     ; still_pict_flag : 1 : save_offset_to (off_4)
     |}
    ->
      let fr = frame_rate_to_string frame_rate in
      [
        Node.make ~offset:off 1 "mfr_flag" (Bits (Bool mfr_flag));
        Node.make ~parsed:fr ~offset:(off + off_1) 4 "frame_rate"
          (Hex (Int frame_rate));
        Node.make ~offset:(off + off_2) 1 "MPEG_1_only_flag" (Bits (Bool true));
        Node.make ~offset:(off + off_3) 1 "constrained_parameter_flag"
          (Bits (Bool const_par_flag));
        Node.make ~offset:(off + off_4) 1 "still_picture_flag"
          (Bits (Bool still_pict_flag));
      ]
  | {| mfr_flag        : 1
     ; frame_rate      : 4 : save_offset_to (off_1)
     ; false           : 1 : save_offset_to (off_2)
     ; const_par_flag  : 1 : save_offset_to (off_3)
     ; still_pict_flag : 1 : save_offset_to (off_4)
     ; profile_lvl_ind : 8 : save_offset_to (off_5)
     ; chroma_format   : 2 : save_offset_to (off_6)
     ; fr_ext_flag     : 1 : save_offset_to (off_7)
     ; reserved        : 5 : save_offset_to (off_8)
     |}
    ->
      let chroma = chroma_format_to_string chroma_format in
      let fr = frame_rate_to_string frame_rate in
      [
        Node.make ~offset:off 1 "mfr_flag" (Bits (Bool mfr_flag));
        Node.make ~parsed:fr ~offset:(off + off_1) 4 "frame_rate"
          (Hex (Int frame_rate));
        Node.make ~offset:(off + off_2) 1 "MPEG_1_only_flag" (Bits (Bool true));
        Node.make ~offset:(off + off_3) 1 "constrained_parameter_flag"
          (Bits (Bool const_par_flag));
        Node.make ~offset:(off + off_4) 1 "still_picture_flag"
          (Bits (Bool still_pict_flag));
        Node.make ~offset:(off + off_5) 8 "profile_and_level_indication"
          (Hex (Int profile_lvl_ind));
        Node.make ~parsed:chroma ~offset:(off + off_6) 2 "chroma_format"
          (Bits (Bool true));
        Node.make ~offset:(off + off_7) 1 "frame_rate_extension_flag"
          (Bits (Bool fr_ext_flag));
        Node.make ~offset:(off + off_8) 5 "reserved" (Bits (Int reserved));
      ]
