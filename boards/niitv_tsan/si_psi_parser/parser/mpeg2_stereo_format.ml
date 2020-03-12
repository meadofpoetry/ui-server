let name = "MPEG2_stereoscopic_video_format_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| true     : 1
     ; arr_type : 7 : save_offset_to (off_1)
     |} ->
      [
        Node.make ~offset:off 1 "stereo_video_arrangement_type"
          (Bits (Bool true));
        Node.make ~offset:(off + off_1) 7 "arrangement_type"
          (Hex (Int arr_type));
      ]
  | {| false    : 1
     ; reserved : 7 : save_offset_to (off_1)
     |} ->
      [
        Node.make ~offset:off 1 "stereo_video_arrangement_type"
          (Bits (Bool false));
        Node.make ~offset:(off + off_1) 7 "reserved" (Hex (Int reserved));
      ]
