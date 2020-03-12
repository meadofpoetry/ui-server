let name = "Stereoscopic_video_info_descriptor"

let parse_factor factor =
  match factor with
  | 0 -> "Forbidden"
  | 1 -> "unspecified"
  | 2 -> "Coded resolution is same as coded resolution of base view"
  | 3 -> "Coded resolution is 3/4 coded resolution of base view"
  | 4 -> "Coded resolution is 2/3 coded resolution of base view"
  | 5 -> "Coded resolution is 1/2 coded resolution of base view"
  | 6 | 7 | 8 -> "reserved"
  | x -> Printf.sprintf "user private %d" x

let parse bs off =
  match%bitstring bs with
  | {| reserved_1    : 7
     ; true          : 1 : save_offset_to (off_1)
     ; reserved_2    : 7 : save_offset_to (off_2)
     ; leftview_flag : 1 : save_offset_to (off_3)
     |}
    ->
      [
        Node.make ~offset:off 7 "reserved" (Bits (Int reserved_1));
        Node.make ~offset:(off + off_1) 1 "base_video_flag" (Bits (Bool true));
        Node.make ~offset:(off + off_2) 7 "reserved" (Bits (Int reserved_2));
        Node.make ~offset:(off + off_3) 1 "leftview_flag"
          (Bits (Bool leftview_flag));
      ]
  | {| reserved_1   : 7
     ; false        : 1 : save_offset_to (off_1)
     ; reserved_2   : 7 : save_offset_to (off_2)
     ; usable_as_2d : 1 : save_offset_to (off_3)
     ; hu_f         : 4 : save_offset_to (off_4)
     ; vu_f         : 4 : save_offset_to (off_5)
     |}
    ->
      let vu = parse_factor vu_f in
      let hu = parse_factor hu_f in
      [
        Node.make ~offset:off 7 "reserved" (Bits (Int reserved_1));
        Node.make ~offset:(off + off_1) 1 "base_video_flag" (Bits (Bool false));
        Node.make ~offset:(off + off_2) 7 "reserved" (Bits (Int reserved_2));
        Node.make ~offset:(off + off_3) 1 "usable_as_2D"
          (Bits (Bool usable_as_2d));
        Node.make ~parsed:hu ~offset:(off + off_4) 4
          "horizontal_upsampling_factor" (Bits (Int hu_f));
        Node.make ~parsed:vu ~offset:(off + off_5) 4
          "vertical_upsampling_factor" (Bits (Int vu_f));
      ]
