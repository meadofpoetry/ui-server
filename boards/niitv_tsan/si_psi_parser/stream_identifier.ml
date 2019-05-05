let name = "stream_identifier_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| comp_tag : 8 |} -> [Node.make ~offset:off 8 "component_tag" (Hex (Int comp_tag))]
