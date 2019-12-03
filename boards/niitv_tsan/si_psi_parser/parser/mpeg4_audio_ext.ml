let name = "MPEG-4_audio_extension_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| true          : 1
     ; reserved      : 3  : save_offset_to (off_1)
     ; num_of_loops  : 4  : save_offset_to (off_2)
     ; audio_profile : num_of_loops * 8 : save_offset_to (off_3), bitstring
     ; asc_size      : 8  : save_offset_to (off_4)
     ; _             : -1 : bitstring
     |}
    ->
      (* TODO the rest here must be decoded considering ISO 1496-3 page 38
         (AudioSpecificConfig), but its kinda messy *)
      let audio_profile =
        Bytes.parse ~offset:(off + off_3) audio_profile "audioProfileLevelIndication"
      in
      [ Node.make ~offset:off 1 "ASC_flag" (Bits (Bool true))
      ; Node.make ~offset:(off + off_1) 3 "reserved" (Bits (Int reserved))
      ; Node.make ~offset:(off + off_2) 4 "num_of_loops" (Dec (Int num_of_loops))
      ; Node.make ~offset:(off + off_3) (num_of_loops * 8) "" (List audio_profile)
      ; Node.make ~offset:(off + off_4) 8 "ASC_size" (Dec (Int asc_size)) ]
  | {| false         : 1
     ; reserved      : 3 : save_offset_to (off_1)
     ; num_of_loops  : 4 : save_offset_to (off_2)
     ; audio_profile : num_of_loops * 8 : save_offset_to (off_3), bitstring
     |}
    ->
      let audio_profile =
        Bytes.parse ~offset:(off + off_3) audio_profile "audioProfileLevelIndication"
      in
      [ Node.make ~offset:off 1 "ASC_flag" (Bits (Bool false))
      ; Node.make ~offset:(off + off_1) 3 "reserved" (Bits (Int reserved))
      ; Node.make ~offset:(off + off_2) 4 "num_of_loops" (Dec (Int num_of_loops))
      ; Node.make
          ~offset:(off + off_3)
          (num_of_loops * 8)
          "audio_profile"
          (List audio_profile) ]
