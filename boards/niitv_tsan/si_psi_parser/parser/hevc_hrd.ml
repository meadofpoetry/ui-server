let name = "HEVC timing and HRD descriptor"

let decode off bs =
  match%bitstring bs with
  | {| hrd_man_valid_flag : 1
     ; reserved_1         : 6 : save_offset_to (off_1)
     ; false              : 1 : save_offset_to (off_2)
     |} ->
    [ Node.make ~offset:off 1 "hrd_management_valid_flag" (Bits (Bool hrd_man_valid_flag))
    ; Node.make ~offset:(off + off_1) 6 "reserved" (Bits (Int reserved_1))
    ; Node.make ~offset:(off + off_2) 1 "picture_and_timing_info_present_flag" (Bits (Bool false)) ]
  | {| hrd_man_valid_flag : 1
     ; reserved_1         : 6  : save_offset_to (off_1)
     ; true               : 1  : save_offset_to (off_2)
     ; true               : 1  : save_offset_to (off_3)
     ; reserved_2         : 7  : save_offset_to (off_4)
     ; num_units_in_tick  : 32 : save_offset_to (off_5)
     |} ->
    [ Node.make ~offset:off 1 "hrd_management_valid_flag" (Bits (Bool hrd_man_valid_flag))
    ; Node.make ~offset:(off + off_1) 6  "reserved" (Bits (Int reserved_1))
    ; Node.make ~offset:(off + off_2) 1  "picture_and_timing_info_present_flag" (Bits (Bool true))
    ; Node.make ~offset:(off + off_3) 1  "90kHz_flag" (Bits (Bool true))
    ; Node.make ~offset:(off + off_4) 7  "reserved" (Bits (Int reserved_2))
    ; Node.make ~offset:(off + off_5) 32 "num_units_in_tick" (Dec (Int32 num_units_in_tick)) ]
  | {| hrd_man_valid_flag : 1
     ; reserved_1         : 6  : save_offset_to (off_1)
     ; true               : 1  : save_offset_to (off_2)
     ; false              : 1  : save_offset_to (off_3)
     ; reserved_2         : 7  : save_offset_to (off_4)
     ; n                  : 32 : save_offset_to (off_5)
     ; k                  : 32 : save_offset_to (off_6)
     ; num_units_in_tick  : 32 : save_offset_to (off_7)
     |} ->
    [ Node.make ~offset:off 1 "hrd_management_valid_flag" (Bits (Bool hrd_man_valid_flag))
    ; Node.make ~offset:(off + off_1) 6  "reserved" (Bits (Int reserved_1))
    ; Node.make ~offset:(off + off_2) 1  "picture_and_timing_info_present_flag" (Bits (Bool true))
    ; Node.make ~offset:(off + off_3) 1  "90kHz_flag" (Bits (Bool false))
    ; Node.make ~offset:(off + off_4) 7  "reserved" (Bits (Int reserved_2))
    ; Node.make ~offset:(off + off_5) 32 "N" (Dec (Int32 n))
    ; Node.make ~offset:(off + off_6) 32 "K" (Dec (Int32 k))
    ; Node.make ~offset:(off + off_7) 32 "num_units_in_tick" (Dec (Int32 num_units_in_tick)) ]
