let name = "SVC extension descriptor"

let parse bs off =
  match%bitstring bs with
  | {| width             : 16
     ; height            : 16 : save_offset_to (off_1)
     ; frame_rate        : 16 : save_offset_to (off_2)
     ; average_bitrate   : 16 : save_offset_to (off_3)
     ; maximum_bitrate   : 16 : save_offset_to (off_4)
     ; dependency_id     : 3  : save_offset_to (off_5)
     ; reserved_1        : 5  : save_offset_to (off_6)
     ; quality_id_start  : 4  : save_offset_to (off_7)
     ; quality_id_end    : 4  : save_offset_to (off_8)
     ; temporal_id_start : 3  : save_offset_to (off_9)
     ; temporal_id_end   : 3  : save_offset_to (off_10)
     ; nsnu_present      : 1  : save_offset_to (off_11)
     ; reserved_2        : 1  : save_offset_to (off_12)
     |}
    ->
      [ Node.make ~offset:off 16 "width" (Dec (Int width))
      ; Node.make ~offset:(off + off_1) 16 "height" (Dec (Int height))
      ; Node.make ~offset:(off + off_2) 16 "frame_rate" (Dec (Int frame_rate))
      ; Node.make ~offset:(off + off_3) 16 "average_bitrate" (Dec (Int average_bitrate))
      ; Node.make ~offset:(off + off_4) 16 "maximum_bitrate" (Dec (Int maximum_bitrate))
      ; Node.make ~offset:(off + off_5) 3 "dependency_id" (Hex (Int dependency_id))
      ; Node.make ~offset:(off + off_6) 5 "reserved" (Bits (Int reserved_1))
      ; Node.make ~offset:(off + off_7) 4 "quality_id_start" (Hex (Int quality_id_start))
      ; Node.make ~offset:(off + off_8) 4 "quality_id_end" (Hex (Int quality_id_end))
      ; Node.make
          ~offset:(off + off_9)
          3
          "temporal_id_start"
          (Hex (Int temporal_id_start))
      ; Node.make ~offset:(off + off_10) 3 "temportal_id_end" (Hex (Int temporal_id_end))
      ; Node.make
          ~offset:(off + off_11)
          1
          "no_sei_nal_unit_present"
          (Bits (Bool nsnu_present))
      ; Node.make ~offset:(off + off_12) 1 "reserved" (Bits (Bool reserved_2)) ]
