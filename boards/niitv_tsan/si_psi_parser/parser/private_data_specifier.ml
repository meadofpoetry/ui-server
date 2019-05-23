let name = "private_data_specifier_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| pds : 32 |} ->
    [Node.make ~offset:off 32 "private_data_specifier" (Hex (Int32 pds))]
