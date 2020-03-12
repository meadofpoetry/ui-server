let name = "cell_frequency_link_descriptor"

let rec decode_second_loop off x =
  if Bitstring.bitstring_length x = 0 then []
  else
    match%bitstring x with
    | {| cell_id_ext : 8
       ; trans_freq  : 32 : save_offset_to (off_1)
       ; rest        : -1 : save_offset_to (off_2), bitstring
       |}
      ->
        let nodes =
          [
            Node.make ~offset:off 8 "cell_id_extension" (Dec (Int cell_id_ext));
            Node.make ~offset:(off + off_1) 32 "transposer_frequency"
              (Dec (Uint32 trans_freq));
          ]
        in
        let name = Printf.sprintf "Subcell %s" (string_of_int cell_id_ext) in
        let node = Node.make ~offset:off 40 name (List nodes) in
        node :: decode_second_loop (off + off_2) rest

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0 then []
  else
    match%bitstring bs with
    | {| cell_id     : 16
       ; frequency   : 32 : save_offset_to (off_1)
       ; loop_length : 8  : save_offset_to (off_2)
       ; snd_loop    : loop_length * 8 : save_offset_to (off_3), bitstring
       ; rest        : -1 : save_offset_to (off_4), bitstring
       |}
      ->
        let name = Printf.sprintf "Cell %s" (string_of_int cell_id) in
        let snd_loop = decode_second_loop (off + off_3) snd_loop in
        let nodes =
          [
            Node.make ~offset:off 16 "cell_id" (Dec (Int cell_id));
            Node.make ~offset:(off + off_1) 32 "frequency"
              (Dec (Uint32 frequency));
            Node.make ~offset:(off + off_2) 8 "subcell_info_loop_length"
              (Dec (Int loop_length));
            Node.make ~offset:(off + off_3) (loop_length * 8) "Subcells"
              (List snd_loop);
          ]
        in
        let real_length = 56 + (loop_length * 8) in
        let node = Node.make ~offset:off real_length name (List nodes) in
        node :: parse rest (off + off_4)
