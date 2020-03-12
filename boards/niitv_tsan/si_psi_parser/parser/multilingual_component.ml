let name = "multilingual_component_descriptor"

let rec parse_rest bs off =
  if Bitstring.bitstring_length bs = 0 then []
  else
    match%bitstring bs with
    | {| lang_code : 24 : bitstring
       ; length    : 8  : save_offset_to (off_1)
       ; descr     : length * 8 : save_offset_to (off_2), bitstring
       ; rest      : -1 : save_offset_to (off_3), bitstring
       |}
      ->
        let parsed, lang_code = Language_code.parse lang_code in
        let text =
          match Text_decoder.decode @@ Util.Bitstring.to_cstruct descr with
          | Ok s -> s
          | Error _ -> "Unable to decode"
        in
        let nodes =
          [
            Node.make ~parsed ~offset:off 24 "ISO_639_language_code"
              (Bits (Int lang_code));
            Node.make ~offset:(off + off_1) 8 "text_description_length"
              (Dec (Int length));
            Node.make ~offset:(off + off_2) (length * 8) "text_description"
              (String text);
          ]
        in
        let real_length = 32 + (length * 8) in
        let node = Node.make ~offset:off real_length parsed (List nodes) in
        node :: parse_rest rest (off + off_3)

let parse bs off =
  match%bitstring bs with
  | {| component_tag : 8
     ; rest          : -1 : save_offset_to (off_1), bitstring
     |}
    ->
      let node =
        Node.make ~offset:off 8 "component_tag" (Hex (Int component_tag))
      in
      node :: parse_rest rest (off + off_1)
