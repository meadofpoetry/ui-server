let name = "adaptation_field_data_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| adapt_field_data_id : 8 |} ->
      [ Node.make
          ~offset:off
          8
          "adaptation_field_data_identifier"
          (Hex (Int adapt_field_data_id)) ]
