let name = "target_background_grid_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| horizontal_size : 14
     ; vertical_size   : 14 : save_offset_to (off_1)
     ; asp_ratio_inf   : 4  : save_offset_to (off_2)
     |}
    ->
      [
        Node.make ~offset:off 14 "horizontal_size" (Dec (Int horizontal_size));
        Node.make ~offset:(off_1 + off) 14 "vertical_size"
          (Dec (Int vertical_size));
        Node.make ~offset:(off_2 + off) 4 "aspect_ratio_information"
          (Hex (Int asp_ratio_inf));
      ]
