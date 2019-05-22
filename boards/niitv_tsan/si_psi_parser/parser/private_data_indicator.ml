let name = "private_data_indicator_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| private_data_indicator : 32 |} ->
    [Node.make ~offset:off 32 "private_data_indicator" (Hex (Int32 private_data_indicator))]
