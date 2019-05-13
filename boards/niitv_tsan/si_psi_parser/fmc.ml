let name = "FMC_descriptor"

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0 then []
  else match%bitstring bs with
    | {| es_id       : 16
       ; flex_mux_ch : 8  : save_offset_to (off_1)
       ; rest        : -1 : save_offset_to (off_2), bitstring
       |} ->
      let nodes =
        [ Node.make ~offset:off 16 "ES_ID" (Hex (Int es_id))
        ; Node.make ~offset:(off + off_1) 8 "FlexMuxChannel" (Hex (Int flex_mux_ch))]
      in
      let id = Printf.sprintf "ES_ID %s" (string_of_int es_id) in
      let node = Node.make ~offset:off 24 id (List nodes) in
      node :: parse rest (off + off_2)
