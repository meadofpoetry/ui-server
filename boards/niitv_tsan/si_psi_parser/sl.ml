let name = "SL_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| es_id : 16 |} -> [Node.make ~offset:off 16 "ES_ID" (Hex (Int es_id))]
