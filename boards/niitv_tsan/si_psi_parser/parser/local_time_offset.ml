let name = "local_time_offset_descriptor"

let parse_country country =
  match country with
  | 0 -> "no time zone extension used"
  | x when x > 0 && x < 61 -> Printf.sprintf "time zone %d" x
  | _ -> "reserved"

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0 then []
  else
    match%bitstring bs with
    | {| country_code     : 24 : bitstring
       ; country_reg_id   : 6  : save_offset_to (off_1)
       ; reserved         : 1  : save_offset_to (off_2)
       ; offset_pol       : 1  : save_offset_to (off_3)
       ; offset           : 16 : save_offset_to (off_4), bitstring
       ; time_of_change   : 40 : save_offset_to (off_5), bitstring
       ; next_time_offset : 16 : save_offset_to (off_6), bitstring
       ; rest             : -1 : save_offset_to (off_7), bitstring
       |}
      ->
        let p_code, country_code = Language_code.parse country_code in
        let time_ch =
          match Date_time.parse_timestamp time_of_change with
          | Some x -> Node.Time x
          | None -> (
              match%bitstring time_of_change with
              | {| i : 40 |} -> Node.Dec (Uint64 i) )
        in
        let local_offset =
          match Date_time.parse_timestamp offset with
          | Some x -> Node.Time x
          | None -> (
              match%bitstring time_of_change with
              | {| i : 40 |} -> Node.Dec (Uint64 i) )
        in
        let nt_offset =
          match Date_time.parse_timestamp next_time_offset with
          | Some x -> Node.Time x
          | None -> (
              match%bitstring time_of_change with
              | {| i : 40 |} -> Node.Dec (Uint64 i) )
        in
        let nodes =
          [
            Node.make ~parsed:p_code ~offset:off 24 "country_code"
              (Bits (Int country_code));
            Node.make ~offset:(off + off_1) 6 "country_reg_id"
              (Hex (Int country_reg_id));
            Node.make ~offset:(off + off_2) 1 "reserved" (Bits (Bool reserved));
            Node.make ~offset:(off + off_3) 1 "local_time_offset_polarity"
              (Bits (Bool offset_pol));
            Node.make ~offset:(off + off_4) 16 "local_time_offset" local_offset;
            Node.make ~offset:(off + off_5) 40 "time_of_change" time_ch;
            Node.make ~offset:(off + off_6) 16 "next_time_offset" nt_offset;
          ]
        in
        let node = Node.make ~offset:off 104 p_code (List nodes) in
        node :: parse rest (off + off_7)
