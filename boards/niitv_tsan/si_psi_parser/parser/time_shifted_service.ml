let name = "time_shifted_service_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| service_id : 16 |} ->
      [Node.make ~offset:off 16 "reference_service_id" (Hex (Int service_id))]
