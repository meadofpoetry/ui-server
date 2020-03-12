let name = "video_window_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| horizontal_offset : 14
     ; vertical_offset   : 14 : save_offset_to (off_1)
     ; window_priority   : 4  : save_offset_to (off_2)
     |}
    ->
      [
        Node.make ~offset:off 14 "horizontal_offset"
          (Dec (Int horizontal_offset));
        Node.make ~offset:(off_1 + off) 14 "vertical_offset"
          (Dec (Int vertical_offset));
        Node.make ~offset:(off_2 + off) 4 "window_priority"
          (Dec (Int window_priority));
      ]
