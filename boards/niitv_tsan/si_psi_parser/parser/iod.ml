let name = "IOD_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| scope_of_iod_label : 8
     ; iod_label          : 8 : save_offset_to (off_1)
     ; initial_obj_desc   : 8 : save_offset_to (off_2)
     |}
    ->
      [
        Node.make ~offset:off 8 "Scope_of_IOD_label"
          (Hex (Int scope_of_iod_label));
        Node.make ~offset:(off_1 + off) 8 "IOD_label" (Hex (Int iod_label));
        Node.make ~offset:(off_2 + off) 8 "InitialObjectDesc"
          (Hex (Int initial_obj_desc));
      ]
