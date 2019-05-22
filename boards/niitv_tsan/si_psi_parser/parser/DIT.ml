open Table_common

let parse = function%bitstring
  | {| header          : 24 : bitstring
     ; transition_flag : 1  : save_offset_to (off_1)
     ; rfu             : 7  : save_offset_to (off_2)
     ; rest            : -1 : bitstring
     |} when Bitstring.bitstring_length rest = 0 ->
    let nodes =
      [ Node.make ~offset:off_1 1 "transition_flag" (Bits (Bool transition_flag))
      ; Node.make ~offset:off_2 7 "reserved_future_use" (Bits (Int rfu))
      ]
    in
    let header = parse_header header in
    header @ nodes
