let name = "bouquet_name_descriptor"

let parse bs off =
  let s =
    match Text_decoder.decode @@ Util.Bitstring.to_cstruct bs with
    | Ok s -> s
    | Error _ -> "Unable to decode"
  in
  [Node.make ~offset:off (Bitstring.bitstring_length bs) "bouquet name" (String s)]
