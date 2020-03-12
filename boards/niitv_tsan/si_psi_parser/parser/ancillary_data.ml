let name = "ancillary_data_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| anc_data_id : 8 |} ->
      [
        Node.make ~offset:off 8 "ancillary_data_identifier"
          (Hex (Int anc_data_id));
      ]
