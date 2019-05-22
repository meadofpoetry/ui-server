let name = "service_list_descriptor"

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0 then []
  else match%bitstring bs with
    | {| service_id   : 16
       ; service_type : 8  : save_offset_to (off_1)
       ; rest         : -1 : save_offset_to (off_2), bitstring
       |} ->
      let parsed = Application_types.MPEG_TS.service_type_to_string service_type in
      let nodes =
        [ Node.make ~offset:off 16 "service_id" (Hex (Int service_id))
        ; Node.make ~parsed ~offset:(off + off_1) 8 "service_type" (Hex (Int service_type)) ]
      in
      let service_name = Printf.sprintf "Service %s" (string_of_int service_id) in
      let node = Node.make ~offset:off 24 service_name (List nodes) in
      node :: parse rest (off + off_2)
