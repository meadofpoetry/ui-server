let name = "registration_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| format_id : 32
     ; rest      : -1 : save_offset_to (off_1), bitstring
     |}
    ->
      let info =
        match Text_decoder.decode @@ Util.Bitstring.to_cstruct rest with
        | Ok s -> s
        | Error _ -> "Unable to decode"
      in
      [
        Node.make ~offset:off 32 "format_identifier" (Hex (Int32 format_id));
        Node.make ~offset:(off + off_1)
          (Bitstring.bitstring_length rest)
          "additional_identification_info" (String info);
      ]
