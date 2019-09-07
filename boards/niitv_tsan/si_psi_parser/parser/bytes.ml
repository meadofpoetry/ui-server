let rec parse ?bytes ~offset x str =
  let bytes =
    match bytes with
    | Some x -> x
    | None -> 1
  in
  if Bitstring.bitstring_length x = 0
  then []
  else
    match%bitstring x with
    | {| smth : bytes * 8
       ; rest : -1 : save_offset_to (off_1), bitstring
       |}
      ->
        let node = Node.make ~offset 8 str (Bits (Int64 smth)) in
        node :: parse ~offset:(offset + off_1) rest str
