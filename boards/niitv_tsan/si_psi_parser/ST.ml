open Table_common

let string_to_list (s : string) =
  let rec _to_list s acc i len =
    if len = 0 then List.rev acc
    else _to_list s (s.[i] :: acc) (i + 1) (len - 1) in
  _to_list s [] 0 (String.length s)

let parse bs =
  match%bitstring bs with
  | {| header : 24 : bitstring
     ; rest   : -1 : bitstring
     |} ->
    let char_list = string_to_list @@ Bitstring.string_of_bitstring rest in
    let int_list = List.map (fun x -> Char.code x) char_list in
    let bytes =
      Node.make
        ~offset:24
        (Bitstring.bitstring_length rest)
        "bytes"
        (Bytes int_list)
    in
    let header = parse_header header in
    header @ [ bytes ]
