let name = "data_broadcast_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| data_broadcast_id : 16
     ; component_tag     : 8  : save_offset_to (off_1)
     ; sel_length        : 8  : save_offset_to (off_2)
     ; selector_bytes    : sel_length * 8 : save_offset_to (off_3), bitstring
     ; lang_code         : 24 : save_offset_to (off_4), bitstring
     ; text_length       : 8  : save_offset_to (off_5)
     ; text_chars        : text_length * 8 : save_offset_to (off_6), bitstring
     |} ->
    let parsed, lang_code = Language_code.parse lang_code in
    let text =
      match Text_decoder.decode @@ Util.Bitstring.to_cstruct text_chars with
      | Ok s -> s
      | Error _ -> "Unable to decode" in
    let nodes_1 =
      [ Node.make ~offset:off 16 "data_broadcast_id" (Hex (Int data_broadcast_id))
      ; Node.make ~offset:(off + off_1) 8 "component_tag" (Hex (Int component_tag))
      ; Node.make ~offset:(off + off_2) 8 "selector_length" (Dec (Int sel_length))
      ]
    in
    (nodes_1 @ Bytes.parse ~offset:(off + off_3) selector_bytes "selector_byte")
    @ [ Node.make ~parsed ~offset:(off + off_4) 24 "ISO_639_language_code" (Hex (Int lang_code))
      ; Node.make ~offset:(off + off_5) 8 "text_length" (Dec (Int text_length))
      ; Node.make ~offset:(off + off_6) (text_length * 8) "text" (String text)
      ]
