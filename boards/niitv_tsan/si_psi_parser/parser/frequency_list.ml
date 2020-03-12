let name = "frequency_list_descriptor"

let parse_coding_type typ =
  match typ with
  | 0 -> "not defined"
  | 1 -> "satellite"
  | 2 -> "cable"
  | 3 -> "terrestrial"
  | _ -> assert false

let parse bs off =
  match%bitstring bs with
  | {| rfu         : 6
     ; coding_type : 2  : save_offset_to (off_1)
     ; rest        : -1 : save_offset_to (off_2), bitstring
     |}
    ->
      let parsed = parse_coding_type coding_type in
      let nodes =
        [
          Node.make ~offset:off 6 "reserved_future_use" (Bits (Int rfu));
          Node.make ~parsed ~offset:(off + off_1) 2 "coding_type"
            (Bits (Int coding_type));
        ]
      in
      nodes @ Bytes.parse ~bytes:4 ~offset:(off + off_2) rest "centre_frequency"
