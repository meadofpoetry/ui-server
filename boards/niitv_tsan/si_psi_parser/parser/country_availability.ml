let name = "country_availability_descriptor"

let rec f off x =
  if Bitstring.bitstring_length x = 0 then []
  else
    match%bitstring x with
    | {| country_code : 24 : bitstring
       ; rest         : -1 : save_offset_to (off_1), bitstring
       |}
      ->
        let parsed, code = Language_code.parse country_code in
        let node =
          Node.make ~parsed ~offset:off 24 "country code" (Bits (Int code))
        in
        node :: f (off + off_1) rest

let parse bs off =
  match%bitstring bs with
  | {| country_avail_flag : 1
     ; rfu                : 7  : save_offset_to (off_1)
     ; rest               : -1 : save_offset_to (off_2), bitstring
     |}
    ->
      let nodes =
        [
          Node.make ~offset:off 1 "country_availability_flag"
            (Bits (Bool country_avail_flag));
          Node.make ~offset:(off + off_1) 7 "reserved_future_use"
            (Bits (Int rfu));
        ]
      in
      nodes @ f (off + off_2) rest
