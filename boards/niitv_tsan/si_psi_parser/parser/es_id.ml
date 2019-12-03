let name = "external_ES_ID descriptor"

let parse bs off =
  match%bitstring bs with
  | {| ext_es_id : 16 |} ->
      [Node.make ~offset:off 16 "External_ES_ID" (Hex (Int ext_es_id))]
