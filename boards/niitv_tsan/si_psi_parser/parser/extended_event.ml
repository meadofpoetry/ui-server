let name = "extended_event_descriptor"

let rec decode_items bs off =
  if Bitstring.bitstring_length bs = 0
  then []
  else
    match%bitstring bs with
    | {| item_descr_length : 8
       ; item_descr        : item_descr_length * 8 : save_offset_to (off_1), bitstring
       ; item_length       : 8  : save_offset_to (off_2)
       ; item              : item_length * 8 : save_offset_to (off_3), bitstring
       ; rest              : -1 : save_offset_to (off_4), bitstring
       |}
      ->
        let item_descr =
          match Text_decoder.decode @@ Util.Bitstring.to_cstruct item_descr with
          | Ok s -> s
          | Error _ -> "Failed to decode"
        in
        let item =
          match Text_decoder.decode @@ Util.Bitstring.to_cstruct item with
          | Ok s -> s
          | Error _ -> "Failed to decode"
        in
        let nodes =
          [ Node.make
              ~offset:off
              8
              "item_description_length"
              (Dec (Int item_descr_length))
          ; Node.make
              ~offset:(off + off_1)
              (item_descr_length * 8)
              "item_description"
              (String item_descr)
          ; Node.make ~offset:(off + off_2) 8 "item_length" (Dec (Int item_length))
          ; Node.make ~offset:(off + off_3) (item_length * 8) "item" (String item) ]
        in
        let real_len = 16 + (item_descr_length * 8) + (item_length * 8) in
        let node = Node.make ~offset:off real_len item (List nodes) in
        node :: decode_items rest (off + off_4)

let parse bs off =
  match%bitstring bs with
  | {| desc_num      : 4
     ; last_desc_num : 4  : save_offset_to (off_1)
     ; lang_code     : 24 : save_offset_to (off_2), bitstring
     ; items_length  : 8  : save_offset_to (off_3)
     ; items         : items_length * 8 : save_offset_to (off_4), bitstring
     ; text_length   : 8  : save_offset_to (off_5)
     ; text          : text_length * 8 : save_offset_to (off_6), bitstring
     |}
    ->
      let items = decode_items items (off + off_4) in
      let text =
        match Text_decoder.decode @@ Util.Bitstring.to_cstruct text with
        | Ok s -> s
        | Error _ -> "Failed to decode"
      in
      let parsed_code, lang_code = Language_code.parse lang_code in
      [ Node.make ~offset:off 4 "descriptor_number" (Dec (Int desc_num))
      ; Node.make ~offset:(off + off_1) 4 "last_desc_num" (Dec (Int last_desc_num))
      ; Node.make
          ~parsed:parsed_code
          ~offset:(off + off_2)
          24
          "ISO_639_language_code"
          (Bits (Int lang_code))
      ; Node.make ~offset:(off + off_3) 8 "items_length" (Dec (Int items_length))
      ; Node.make ~offset:(off + off_4) (items_length * 8) "items" (List items)
      ; Node.make ~offset:(off + off_5) 8 "text_length" (Dec (Int text_length))
      ; Node.make ~offset:(off + off_6) (text_length * 8) "text" (String text) ]
