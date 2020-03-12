let name = "data_stream_alignment_descriptor"

let alignment_to_string alignment =
  match alignment with
  | 00 -> "Reserved"
  | 01 -> "Slice, or video access unit"
  | 02 -> "Video access unit"
  | 03 -> "GOP, or SEQ"
  | 04 -> "SEQ"
  | _ -> "Reserved"

let parse bs off =
  match%bitstring bs with
  | {| alignment_type : 8 |} ->
      let parsed = alignment_to_string alignment_type in
      [
        Node.make ~parsed ~offset:off 1 "alignment_type"
          (Hex (Int alignment_type));
      ]
