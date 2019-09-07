let audio_to_string = function
  | 0x00 -> "Undefined"
  | 0x01 -> "Clean_effects"
  | 0x02 -> "Hearing_impaired"
  | 0x03 -> "Visual_impaired_commentary"
  | x when x >= 0x04 && x <= 0x7F -> "User_private x"
  | _ -> "Reserved"

let name = "ISO_639_language_descriptor"

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0
  then []
  else
    match%bitstring bs with
    | {| lang_code : 24 : bitstring
       ; audio     : 8  : save_offset_to (off_1)
       ; rest      : -1 : save_offset_to (off_2), bitstring
       |}
      ->
        let typ = audio_to_string audio in
        let lang, lang_code = Language_code.parse lang_code in
        let nodes =
          [ Node.make
              ~parsed:lang
              ~offset:off
              24
              "ISO_639_language_code"
              (Bits (Int lang_code))
          ; Node.make ~parsed:typ ~offset:(off_1 + off) 8 "audio_type" (Hex (Int audio))
          ]
        in
        let node = Node.make ~offset:off 32 lang (List nodes) in
        node :: parse rest (off + off_2)
