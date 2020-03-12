(* refers to ETSI TS 102 809 *)

let name = "application_signalling_descriptor"

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0 then []
  else
    match%bitstring bs with
    | {| rfu_1       : 1
       ; app_type    : 15 : save_offset_to (off_1)
       ; rfu_2       : 3  : save_offset_to (off_2)
       ; ait_ver_num : 5  : save_offset_to (off_3)
       ; rest        : -1 : save_offset_to (off_4), bitstring
       |}
      ->
        let node_list =
          [
            Node.make ~offset:off 1 "reserved_future_use" (Bits (Bool rfu_1));
            Node.make ~offset:(off + off_1) 15 "application_type"
              (Hex (Int app_type));
            Node.make ~offset:(off + off_2) 3 "reserved_future_use"
              (Bits (Int rfu_2));
            Node.make ~offset:(off + off_3) 5 "AIT_version_number"
              (Dec (Int ait_ver_num));
          ]
        in
        let node =
          [ Node.make ~offset:off 24 "application signalling" (List node_list) ]
        in
        node @ parse rest (off + off_4)
