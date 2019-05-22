(* ETSI TS 102 323  page 101 *)

let name = "content_identifier_descriptor"

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0 then []
  else match%bitstring bs with
    | {| crid_type   : 6
       ; 00          : 2  : save_offset_to (off_1)
       ; crid_length : 8  : save_offset_to (off_2)
       ; crid        : crid_length * 8 : save_offset_to (off_3), bitstring
       ; rest        : -1 : save_offset_to (off_4), bitstring
       |} ->
      let crid =
        match Text_decoder.decode @@ Util.Bitstring.to_cstruct crid with
        | Ok s -> s
        | Error _ -> "Unable to decode" in
      let nodes =
        [ Node.make ~offset:off 6 "crid_type" (Dec (Int crid_type))
        ; Node.make ~offset:(off + off_1) 2 "crid_location" (Bits (Int 00))
        ; Node.make ~offset:(off + off_2) 8 "crid_length" (Dec (Int crid_length))
        ; Node.make ~offset:(off + off_3) (crid_length * 8) "crid" (String crid) ]
      in
      let real_length = 16 + crid_length * 8 in
      let node = Node.make ~offset:off real_length crid (List nodes) in
      node :: parse rest (off + off_4)
    | {| crid_type   : 6
       ; 01          : 2  : save_offset_to (off_1)
       ; crid_ref    : 16 : save_offset_to (off_2)
       ; rest        : -1 : save_offset_to (off_3), bitstring
       |} ->
      let nodes =
        [ Node.make ~offset:off 6 "crid_type" (Dec (Int crid_type))
        ; Node.make ~offset:(off + off_1) 2 "crid_location" (Bits (Int 00))
        ; Node.make ~offset:(off + off_2) 16 "crid_ref" (Dec (Int crid_ref)) ]
      in
      let node = Node.make ~offset:off 24 "identifier" (List nodes) in
      node :: parse rest (off + off_3)
    | {| crid_type     : 6
       ; crid_location : 2  : save_offset_to (off_1)
       ; rest          : -1 : save_offset_to (off_2), bitstring
       |} ->
      let nodes =
        [ Node.make ~offset:off 6 "crid_type" (Dec (Int crid_type))
        ; Node.make ~offset:(off + off_1) 2 "crid_location" (Bits (Int crid_location)) ]
      in
      let node = Node.make ~offset:off 8 "identifier" (List nodes) in
      node :: parse rest (off + off_2)
