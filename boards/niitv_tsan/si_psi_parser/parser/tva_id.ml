(* ETSI TS 102 323 *)

let name = "TVA_id_descriptor"

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0 then []
  else
    match%bitstring bs with
    | {| tva_id   : 16
       ; reserved : 5  : save_offset_to (off_1)
       ; status   : 3  : save_offset_to (off_2)
       ; rest     : -1 : save_offset_to (off_3), bitstring
       |}
      ->
        let node_list =
          [
            Node.make ~offset:off 16 "TVA_id" (Dec (Int tva_id));
            Node.make ~offset:(off + off_1) 5 "reserved" (Bits (Int reserved));
            Node.make ~offset:(off + off_2) 3 "running_status"
              (Dec (Int status));
          ]
        in
        let tva_name = Printf.sprintf "TVA_id %s" (string_of_int tva_id) in
        let node = Node.make ~offset:off 24 tva_name (List node_list) in
        node :: parse rest (off + off_3)
