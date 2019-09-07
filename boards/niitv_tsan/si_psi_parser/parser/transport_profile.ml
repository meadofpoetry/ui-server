let name = "Transport_profile_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| transport_profile : 8
     ; rest              : -1 : save_offset_to (off_1), bitstring
     |}
    ->
      let node =
        Node.make ~offset:off 8 "transport_profile" (Hex (Int transport_profile))
      in
      node :: Bytes.parse ~offset:(off + off_1) rest "private_data"
