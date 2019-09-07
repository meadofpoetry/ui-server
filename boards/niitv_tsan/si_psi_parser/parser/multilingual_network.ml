let name = "multilingual_network_name_descriptor"

let rec f off x =
  if Bitstring.bitstring_length x = 0
  then []
  else
    match%bitstring x with
    | {| lang_code : 24 : bitstring
       ; length    : 8  : save_offset_to (off_1)
       ; network   : length * 8 : save_offset_to (off_2), bitstring
       ; rest      : -1 : save_offset_to (off_3), bitstring
       |}
      ->
        let parsed, lang_code = Language_code.parse lang_code in
        let network_name =
          match Text_decoder.decode @@ Util.Bitstring.to_cstruct network with
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
          ; Node.make ~offset:(off + off_1) 8 "network_name_length" (Dec (Int length))
          ; Node.make
              ~offset:(off + off_2)
              (length * 8)
              "network_name"
              (String network_name) ]
        in
        let node = Node.make ~offset:off (32 + (length * 8)) parsed (List nodes) in
        node :: f (off + off_3) rest

let parse bs off = f off bs
