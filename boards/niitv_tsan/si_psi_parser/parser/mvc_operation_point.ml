let name = "MVC operation point descriptor"

let rec parse_recommendations off x =
  if Bitstring.bitstring_length x = 0
  then []
  else
    match%bitstring x with
    | {| level_idc    : 8
       ; points_count : 8  : save_offset_to (off_1)
       ; rest         : -1 : save_offset_to (off_2), bitstring
       |}
      ->
        let nodes =
          [ Node.make ~offset:off 8 "level_idc" (Hex (Int level_idc))
          ; Node.make
              ~offset:(off + off_1)
              8
              "operation_points_count"
              (Hex (Int points_count)) ]
        in
        let level_name = Printf.sprintf "Level %s" (string_of_int level_idc) in
        let node = Node.make ~offset:off 16 level_name (List nodes) in
        node :: parse_recommendations (off + off_2) rest

(* FIXME *)

let parse bs off =
  match%bitstring bs with
  | {| profile_idc          : 8
     ; constraint_set0_flag : 1  : save_offset_to (off_1)
     ; constraint_set1_flag : 1  : save_offset_to (off_2)
     ; constraint_set2_flag : 1  : save_offset_to (off_3)
     ; constraint_set3_flag : 1  : save_offset_to (off_4)
     ; constraint_set4_flag : 1  : save_offset_to (off_5)
     ; constraint_set5_flag : 1  : save_offset_to (off_6)
     ; avc_compatible_flags : 2  : save_offset_to (off_7)
     ; level_count          : 8  : save_offset_to (off_8)
     ; rest                 : -1 : save_offset_to (off_9), bitstring
     |}
    ->
      let nodes =
        [ Node.make ~offset:off 8 "profile_and_level" (Hex (Int profile_idc))
        ; Node.make
            ~offset:(off + off_1)
            1
            "constraint_set0_flag"
            (Bits (Bool constraint_set0_flag))
        ; Node.make
            ~offset:(off + off_2)
            1
            "constraint_set1_flag"
            (Bits (Bool constraint_set1_flag))
        ; Node.make
            ~offset:(off + off_3)
            1
            "constraint_set2_flag"
            (Bits (Bool constraint_set2_flag))
        ; Node.make
            ~offset:(off + off_4)
            1
            "constraint_set3_flag"
            (Bits (Bool constraint_set3_flag))
        ; Node.make
            ~offset:(off + off_5)
            1
            "constraint_set4_flag"
            (Bits (Bool constraint_set4_flag))
        ; Node.make
            ~offset:(off + off_6)
            1
            "constraint_set5_flag"
            (Bits (Bool constraint_set5_flag))
        ; Node.make
            ~offset:(off + off_7)
            2
            "AVC_compatible_flags"
            (Bits (Int avc_compatible_flags))
        ; Node.make ~offset:(off + off_8) 8 "level_count" (Hex (Int level_count)) ]
      in
      nodes @ parse_recommendations (off + off_9) rest
