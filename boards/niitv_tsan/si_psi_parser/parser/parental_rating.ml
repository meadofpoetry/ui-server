let name = "patental_rating_descriptor"

let parse_rating rating =
  match rating with
  | 0x00 -> "undefined"
  | x when x > 0x00 && x < 0x10 -> Printf.sprintf "%d+" (x + 3)
  | _ -> "defined by the broadcaster"

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0
  then []
  else
    match%bitstring bs with
    | {| country_code : 24 : bitstring
       ; rating       : 8  : save_offset_to (off_1)
       ; rest         : -1 : save_offset_to (off_2), bitstring
       |}
      ->
        let p_rating = parse_rating rating in
        let p_code, country_code = Language_code.parse country_code in
        let nodes =
          [ Node.make
              ~parsed:p_code
              ~offset:off
              24
              "country_code"
              (Bits (Int country_code))
          ; Node.make
              ~parsed:p_rating
              ~offset:(off + off_1)
              8
              "rating"
              (Dec (Int rating)) ]
        in
        let node = Node.make ~offset:off 32 p_code (List nodes) in
        node :: parse rest (off + off_2)
