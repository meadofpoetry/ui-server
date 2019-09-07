let name = "AVC video descriptor"

let parse bs off =
  match%bitstring bs with
  | {| profile_idc          : 8
     ; constraint_set0_flag : 1 : save_offset_to (off_1)
     ; constraint_set1_flag : 1 : save_offset_to (off_2)
     ; constraint_set2_flag : 1 : save_offset_to (off_3)
     ; constraint_set3_flag : 1 : save_offset_to (off_4)
     ; constraint_set4_flag : 1 : save_offset_to (off_5)
     ; constraint_set5_flag : 1 : save_offset_to (off_6)
     ; avc_compatible_flags : 2 : save_offset_to (off_7)
     ; level_idc            : 8 : save_offset_to (off_8)
     ; avc_still_present    : 1 : save_offset_to (off_9)
     ; avc_24_hr_pict_flag  : 1 : save_offset_to (off_10)
     ; fr_flag              : 1 : save_offset_to (off_11)
     ; reserved             : 5 : save_offset_to (off_12)
     |}
    ->
      [ Node.make ~offset:off 8 "profile_idc" (Hex (Int profile_idc))
      ; Node.make
          ~offset:(off + off_1)
          1
          "constraint_set0_flag"
          (Bits (Bool constraint_set0_flag))
      ; Node.make
          ~offset:(off + off_2)
          1
          "constraint_set1_flag"
          (Bits (Bool constraint_set1_flag))
      ; Node.make
          ~offset:(off + off_3)
          1
          "constraint_set2_flag"
          (Bits (Bool constraint_set2_flag))
      ; Node.make
          ~offset:(off + off_4)
          1
          "constraint_set3_flag"
          (Bits (Bool constraint_set3_flag))
      ; Node.make
          ~offset:(off + off_5)
          1
          "constraint_set4_flag"
          (Bits (Bool constraint_set4_flag))
      ; Node.make
          ~offset:(off + off_6)
          1
          "constraint_set5_flag"
          (Bits (Bool constraint_set5_flag))
      ; Node.make
          ~offset:(off + off_7)
          2
          "AVC_compatible_flags"
          (Bits (Int avc_compatible_flags))
      ; Node.make ~offset:(off + off_8) 8 "level_idc" (Hex (Int level_idc))
      ; Node.make
          ~offset:(off + off_9)
          1
          "AVC_still_present"
          (Bits (Bool avc_still_present))
      ; Node.make
          ~offset:(off + off_10)
          1
          "AVC_24_hour_picture_flag"
          (Bits (Bool avc_24_hr_pict_flag))
      ; Node.make
          ~offset:(off + off_11)
          1
          "Frame_Packing_SEI_not_present_flag"
          (Bits (Bool fr_flag))
      ; Node.make ~offset:(off + off_12) 5 "reserved" (Bits (Int reserved)) ]
