let name = "subtitling_descriptor"

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0
  then []
  else
    match%bitstring bs with
    | {| lang_code : 24 : bitstring
       ; sub_type  : 8  : save_offset_to (off_1)
       ; cp_id     : 16 : save_offset_to (off_2)
       ; ap_id     : 16 : save_offset_to (off_3)
       ; rest      : -1 : save_offset_to (off_4), bitstring
       |}
      ->
        let parsed, lang_code = Language_code.parse lang_code in
        let nodes =
          [ Node.make
              ~parsed
              ~offset:off
              24
              "ISO_639_language_code"
              (Bits (Int lang_code))
          ; Node.make ~offset:(off + off_1) 8 "subtitling_type" (Hex (Int sub_type))
          ; Node.make ~offset:(off + off_2) 16 "composition_page_id" (Hex (Int cp_id))
          ; Node.make ~offset:(off + off_3) 16 "ancillary_page_id" (Hex (Int ap_id)) ]
        in
        let node = Node.make ~offset:off 64 parsed (List nodes) in
        node :: parse rest (off + off_4)
