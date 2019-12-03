(* FIXME this might be working, but should be tested carefully*)

let name = "mosaic_descriptor"

let parse_present_info inf =
  match inf with
  | 0 -> "undefined"
  | 1 -> "video"
  | 2 -> "still picture"
  | 3 -> "graphics/text"
  | _ -> "reserved for future use"

let parse_cli cli =
  match cli with
  | 0 -> "undefined"
  | 1 -> "bouquet related"
  | 2 -> "service related"
  | 3 -> "other mosaic related"
  | 4 -> "event related"
  | _ -> "reserved for future use"

let parse_cells num =
  match num with
  | 0 -> "one cell"
  | 1 -> "two cells"
  | 2 -> "three cells"
  | 3 -> "four cells"
  | 4 -> "five cells"
  | 5 -> "six cells"
  | 6 -> "seven cells"
  | 7 -> "eight cells"
  | _ -> assert false

let further ~cli ~offset _ x =
  (* FIXME acc *)
  match cli with
  | 0x01 -> (
      match%bitstring x with
      | {| bouquet_id : 16
         ; rest       : -1 : save_offset_to (off_1), bitstring
         |}
        ->
          ( [Node.make ~offset 16 "bouquet_id" (Hex (Int bouquet_id))]
          , rest
          , offset + off_1 ))
  | 0x02 | 0x03 -> (
      match%bitstring x with
      | {| on_id : 16
         ; ts_id : 16 : save_offset_to (off_1)
         ; sv_id : 16 : save_offset_to (off_2)
         ; rest  : -1 : save_offset_to (off_3), bitstring
         |}
        ->
          ( [ Node.make ~offset 16 "original_network_id" (Hex (Int on_id))
            ; Node.make
                ~offset:(offset + off_1)
                16
                "transport_stream_id"
                (Hex (Int ts_id))
            ; Node.make ~offset:(offset + off_2) 16 "service_id" (Hex (Int sv_id)) ]
          , rest
          , offset + off_3 ))
  | 0x04 -> (
      match%bitstring x with
      | {| on_id : 16
         ; ts_id : 16 : save_offset_to (off_1)
         ; sv_id : 16 : save_offset_to (off_2)
         ; ev_id : 16 : save_offset_to (off_3)
         ; rest  : -1 : save_offset_to (off_4), bitstring
         |}
        ->
          ( [ Node.make ~offset 16 "original_network_id" (Hex (Int on_id))
            ; Node.make
                ~offset:(offset + off_1)
                16
                "transport_stream_id"
                (Hex (Int ts_id))
            ; Node.make ~offset:(offset + off_2) 16 "service_id" (Hex (Int sv_id))
            ; Node.make ~offset:(offset + off_3) 16 "event_id" (Hex (Int ev_id)) ]
          , rest
          , offset + off_4 ))
  | _ -> assert false

let rec f_2 off x =
  if Bitstring.bitstring_length x = 0
  then []
  else
    match%bitstring x with
    | {| rfu                : 2
       ; elementary_cell_id : 6  : save_offset_to (off_1)
       ; rest               : -1 : save_offset_to (off_2), bitstring
       |}
      ->
        let nodes =
          [ Node.make ~offset:off 2 "reserved_future_use" (Bits (Int rfu))
          ; Node.make
              ~offset:(off + off_1)
              6
              "elementary_cell_id"
              (Dec (Int elementary_cell_id)) ]
        in
        nodes @ f_2 (off + off_2) rest

let rec f off x =
  if Bitstring.bitstring_length x = 0
  then []
  else
    match%bitstring x with
    | {| logical_cell_id : 6
       ; rfu             : 7  : save_offset_to (off_1)
       ; pr_info         : 3  : save_offset_to (off_2)
       ; elem_len        : 8  : save_offset_to (off_3)
       ; elem_cell_field : elem_len * 8 : save_offset_to (off_4), bitstring
       ; cli             : 8  : save_offset_to (off_5)
       ; rest            : -1 : save_offset_to (off_6), bitstring
       |}
      ->
        let parsed = parse_present_info pr_info in
        let nodes =
          [ Node.make ~offset:off 6 "logical_cell_id" (Hex (Int logical_cell_id))
          ; Node.make ~offset:(off + off_1) 7 "reserved_future_use" (Bits (Int rfu))
          ; Node.make
              ~parsed
              ~offset:(off + off_2)
              3
              "logical_cell_presentation_info"
              (Bits (Int pr_info))
          ; Node.make
              ~offset:(off + off_3)
              8
              "elementary_cell_field_length"
              (Dec (Int elem_len)) ]
        in
        let nodes = nodes @ f_2 (off + off_4) elem_cell_field in
        let cli_parsed = parse_cli cli in
        let cli_node =
          [ Node.make
              ~parsed:cli_parsed
              ~offset:(off + off_5)
              8
              "cell_linkage_info"
              (Hex (Int cli)) ]
        in
        let nodes = nodes @ cli_node in
        let nodes, rest, off_7 = further ~cli ~offset:(off + off_6) nodes rest in
        nodes @ f (off + off_7) rest

let parse bs off =
  match%bitstring bs with
  | {| mosaic_entry_point : 1
     ; hor                : 3  : save_offset_to (off_1)
     ; rfu                : 1  : save_offset_to (off_2)
     ; ver                : 3  : save_offset_to (off_3)
     ; rest               : -1 : save_offset_to (off_4), bitstring
     |}
    ->
      let ver_cells = parse_cells ver in
      let hor_cells = parse_cells hor in
      let nodes =
        [ Node.make ~offset:off 1 "mosaic_entry_point" (Bits (Bool mosaic_entry_point))
        ; Node.make
            ~parsed:hor_cells
            ~offset:(off + off_1)
            3
            "number_of_horizontal_elementary_cells"
            (Dec (Int hor))
        ; Node.make ~offset:(off + off_2) 1 "reserved_future_use" (Bits (Bool rfu))
        ; Node.make
            ~parsed:ver_cells
            ~offset:(off + off_3)
            3
            "number_of_vertical_elementary_cells"
            (Dec (Int ver)) ]
      in
      nodes @ f (off + off_4) rest
