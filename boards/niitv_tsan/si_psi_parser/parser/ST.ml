open Table_common

let parse bs =
  match%bitstring bs with
  | {| header : 24 : bitstring
     ; rest   : -1 : bitstring
     |} ->
      let char_list =
        List.of_seq @@ String.to_seq @@ Bitstring.string_of_bitstring rest
      in
      let int_list = List.map (fun x -> Char.code x) char_list in
      let bytes =
        Node.make ~offset:24 (Bitstring.bitstring_length rest) "bytes" (Bytes int_list)
      in
      let header = parse_header header in
      header @ [bytes]
