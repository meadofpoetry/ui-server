open Table_common

let parse = function%bitstring
  | {| header   : 24 : bitstring
     ; utc_time : 40 : save_offset_to (off), bitstring
     |} ->
    let utc_time = match Date_time.parse_timestamp utc_time with
      | Some x -> Node.Time x
      | None -> match%bitstring utc_time with
        | {| i : 40 |} -> Node.Dec (Uint64 i) in
    let node = [Node.make ~offset:off 40 "utc_time" utc_time] in
    let header = parse_header header in
    header @ node
