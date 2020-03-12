let name = "multilingual_service_name_descriptor"

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0 then []
  else
    match%bitstring bs with
    | {| lang_code : 24 : bitstring
       ; length_1  : 8  : save_offset_to (off_1)
       ; service_p : length_1 * 8 : save_offset_to (off_2), bitstring
       ; length_2  : 8  : save_offset_to (off_3)
       ; service   : length_2 * 8 : save_offset_to (off_4), bitstring
       ; rest      : -1 : save_offset_to (off_5), bitstring
       |}
      ->
        let parsed, lang_code = Language_code.parse lang_code in
        let ser_p =
          match Text_decoder.decode @@ Util.Bitstring.to_cstruct service_p with
          | Ok s -> s
          | Error _ -> "Unable to decode"
        in
        let ser =
          match Text_decoder.decode @@ Util.Bitstring.to_cstruct service with
          | Ok s -> s
          | Error _ -> "Unable to decode"
        in
        let nodes =
          [
            Node.make ~parsed ~offset:off 24 "ISO_639_language_code"
              (Bits (Int lang_code));
            Node.make ~offset:(off + off_1) 8 "service_provider_name_length"
              (Dec (Int length_1));
            Node.make ~offset:(off + off_2) (length_1 * 8)
              "service_provider_name" (String ser_p);
            Node.make ~offset:(off + off_3) 8 "service_name_length"
              (Dec (Int length_2));
            Node.make ~offset:(off + off_4) (length_2 * 8) "service__name"
              (String ser);
          ]
        in
        let real_length = 40 + ((length_1 + length_2) * 8) in
        let node = Node.make ~offset:off real_length parsed (List nodes) in
        node :: parse rest (off + off_5)
