let name = "multilingual_bouquet_name_descriptor"

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0
  then []
  else
    match%bitstring bs with
    | {| lang_code : 24 : bitstring
       ; length    : 8  : save_offset_to (off_1)
       ; bouquet   : length * 8 : save_offset_to (off_2), bitstring
       ; rest      : -1 : save_offset_to (off_3), bitstring
       |}
      ->
        let parsed, lang_code = Language_code.parse lang_code in
        let name =
          match Text_decoder.decode @@ Util.Bitstring.to_cstruct bouquet with
          | Ok s -> s
          | Error _ -> "Unable to decode"
        in
        let nodes =
          [ Node.make
              ~parsed
              ~offset:off
              24
              "ISO_639_language_code"
              (Bits (Int lang_code))
          ; Node.make ~offset:(off + off_1) 8 "bouquet_name_length" (Dec (Int length))
          ; Node.make ~offset:(off + off_2) (length * 8) "bouquet_name" (String name) ]
        in
        let node = Node.make ~offset:off (32 + (length * 8)) parsed (List nodes) in
        node :: parse rest (off + off_3)
