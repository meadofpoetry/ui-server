let name = "AVC timing and HRD descriptor"

let parse bs off =
  match%bitstring bs with
  | {| hrd_mng_valid_flag   : 1
     ; reserved_1           : 6  : save_offset_to (off_1)
     ; true                 : 1  : save_offset_to (off_2)
     ; false                : 1  : save_offset_to (off_3)
     ; reserved_2           : 7  : save_offset_to (off_4)
     ; n                    : 32 : save_offset_to (off_5)
     ; k                    : 32 : save_offset_to (off_6)
     ; num_units_in_tick    : 32 : save_offset_to (off_7)
     ; fixed_fr_rate_flag   : 1  : save_offset_to (off_8)
     ; temporal_poc_flag    : 1  : save_offset_to (off_9)
     ; pict_to_display_flag : 1  : save_offset_to (off_10)
     ; reserved_3           : 5  : save_offset_to (off_11)
     |}
    ->
      [
        Node.make ~offset:off 1 "hrd_management_valid_flag"
          (Bits (Bool hrd_mng_valid_flag));
        Node.make ~offset:(off + off_1) 6 "reserved" (Bits (Int reserved_1));
        Node.make ~offset:(off + off_2) 1 "picture_and_timing_info_present"
          (Bits (Bool true));
        Node.make ~offset:(off + off_3) 1 "90kHz_flag" (Bits (Bool false));
        Node.make ~offset:(off + off_4) 7 "reserved" (Bits (Int reserved_2));
        Node.make ~offset:(off + off_5) 32 "N" (Dec (Int32 n));
        Node.make ~offset:(off + off_6) 32 "K" (Dec (Int32 k));
        Node.make ~offset:(off + off_7) 32 "num_units_in_tick"
          (Bits (Int32 num_units_in_tick));
        Node.make ~offset:(off + off_8) 1 "fixed_frame_rate_flag"
          (Bits (Bool fixed_fr_rate_flag));
        Node.make ~offset:(off + off_9) 1 "temporal_poc_flag"
          (Bits (Bool temporal_poc_flag));
        Node.make ~offset:(off + off_10) 1 "picture_to_display_conversion_flag"
          (Bits (Bool pict_to_display_flag));
        Node.make ~offset:(off + off_11) 5 "reserved" (Bits (Int reserved_3));
      ]
  | {| hrd_mng_valid_flag   : 1
     ; reserved_1           : 6  : save_offset_to (off_1)
     ; true                 : 1  : save_offset_to (off_2)
     ; true                 : 1  : save_offset_to (off_3)
     ; reserved_2           : 7  : save_offset_to (off_4)
     ; num_units_in_tick    : 32 : save_offset_to (off_5)
     ; fixed_fr_rate_flag   : 1  : save_offset_to (off_6)
     ; temporal_poc_flag    : 1  : save_offset_to (off_7)
     ; pict_to_display_flag : 1  : save_offset_to (off_8)
     ; reserved_3           : 5  : save_offset_to (off_9)
     |}
    ->
      [
        Node.make ~offset:off 1 "hrd_management_valid_flag"
          (Bits (Bool hrd_mng_valid_flag));
        Node.make ~offset:(off + off_1) 6 "reserved" (Bits (Int reserved_1));
        Node.make ~offset:(off + off_2) 1 "picture_and_timing_info_present"
          (Bits (Bool true));
        Node.make ~offset:(off + off_3) 1 "90kHz_flag" (Bits (Bool true));
        Node.make ~offset:(off + off_4) 7 "reserved" (Bits (Int reserved_2));
        Node.make ~offset:(off + off_5) 32 "num_units_in_tick"
          (Bits (Int32 num_units_in_tick));
        Node.make ~offset:(off + off_6) 1 "fixed_frame_rate_flag"
          (Bits (Bool fixed_fr_rate_flag));
        Node.make ~offset:(off + off_7) 1 "temporal_poc_flag"
          (Bits (Bool temporal_poc_flag));
        Node.make ~offset:(off + off_8) 1 "picture_to_display_conversion_flag"
          (Bits (Bool pict_to_display_flag));
        Node.make ~offset:(off + off_9) 5 "reserved" (Bits (Int reserved_3));
      ]
  | {| hrd_mng_valid_flag   : 1
     ; reserved_1           : 6 : save_offset_to (off_1)
     ; false                : 1 : save_offset_to (off_2)
     ; fixed_fr_rate_flag   : 1 : save_offset_to (off_3)
     ; temporal_poc_flag    : 1 : save_offset_to (off_4)
     ; pict_to_display_flag : 1 : save_offset_to (off_5)
     ; reserved_2           : 5 : save_offset_to (off_6)
     |}
    ->
      [
        Node.make ~offset:off 1 "hrd_management_valid_flag"
          (Bits (Bool hrd_mng_valid_flag));
        Node.make ~offset:(off + off_1) 6 "reserved" (Bits (Int reserved_1));
        Node.make ~offset:(off + off_2) 1 "picture_and_timing_info_present"
          (Bits (Bool false));
        Node.make ~offset:(off + off_3) 1 "fixed_frame_rate_flag"
          (Bits (Bool fixed_fr_rate_flag));
        Node.make ~offset:(off + off_4) 1 "temporal_poc_flag"
          (Bits (Bool temporal_poc_flag));
        Node.make ~offset:(off + off_5) 1 "picture_to_display_conversion_flag"
          (Bits (Bool pict_to_display_flag));
        Node.make ~offset:(off + off_6) 5 "reserved" (Bits (Int reserved_2));
      ]
