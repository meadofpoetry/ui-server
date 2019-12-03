let name = "telephone_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| rfu_1          : 2
     ; foreign_avail  : 1 : save_offset_to (off_1)
     ; connection     : 5 : save_offset_to (off_2)
     ; rfu_2          : 1 : save_offset_to (off_3)
     ; length_1       : 2 : save_offset_to (off_4)
     ; length_2       : 3 : save_offset_to (off_5)
     ; length_3       : 2 : save_offset_to (off_6)
     ; rfu_3          : 1 : save_offset_to (off_7)
     ; length_4       : 3 : save_offset_to (off_8)
     ; length_5       : 4 : save_offset_to (off_9)
     ; country_prefix : length_1 * 8 : save_offset_to (off_10), bitstring
     ; int_area_code  : length_2 * 8 : save_offset_to (off_11), bitstring
     ; operator_code  : length_3 * 8 : save_offset_to (off_12), bitstring
     ; nat_area_code  : length_4 * 8 : save_offset_to (off_13), bitstring
     ; core_number    : length_5 * 8 : save_offset_to (off_14), bitstring
     |}
    ->
      let country_prefix =
        match Text_decoder.decode @@ Util.Bitstring.to_cstruct country_prefix with
        | Ok s -> s
        | Error _ -> "Unable to decode"
      in
      let int_code =
        match Text_decoder.decode @@ Util.Bitstring.to_cstruct int_area_code with
        | Ok s -> s
        | Error _ -> "Unable to decode"
      in
      let operator_code =
        match Text_decoder.decode @@ Util.Bitstring.to_cstruct operator_code with
        | Ok s -> s
        | Error _ -> "Unable to decode"
      in
      let nat_area_code =
        match Text_decoder.decode @@ Util.Bitstring.to_cstruct nat_area_code with
        | Ok s -> s
        | Error _ -> "Unable to decode"
      in
      let core_number =
        match Text_decoder.decode @@ Util.Bitstring.to_cstruct core_number with
        | Ok s -> s
        | Error _ -> "Unable to decode"
      in
      [ Node.make ~offset:off 2 "reserved_future_use" (Bits (Int rfu_1))
      ; Node.make
          ~offset:(off + off_1)
          1
          "foreign_availability"
          (Bits (Bool foreign_avail))
      ; Node.make ~offset:(off + off_2) 5 "connection_type" (Hex (Int connection))
      ; Node.make ~offset:(off + off_3) 1 "reserved_future_use" (Bits (Bool rfu_2))
      ; Node.make ~offset:(off + off_4) 2 "country_prefix_length" (Dec (Int length_1))
      ; Node.make
          ~offset:(off + off_5)
          3
          "international_area_code_length"
          (Dec (Int length_2))
      ; Node.make ~offset:(off + off_6) 2 "operator_code_length" (Dec (Int length_3))
      ; Node.make ~offset:(off + off_7) 1 "reserved_future_use" (Bits (Bool rfu_3))
      ; Node.make
          ~offset:(off + off_8)
          3
          "national_area_code_length"
          (Dec (Int length_4))
      ; Node.make ~offset:(off + off_9) 4 "core_number_length" (Dec (Int length_5))
      ; Node.make
          ~offset:(off + off_10)
          (length_1 * 8)
          "country_prefix"
          (String country_prefix)
      ; Node.make
          ~offset:(off + off_11)
          (length_2 * 8)
          "international_area_code"
          (String int_code)
      ; Node.make
          ~offset:(off + off_12)
          (length_3 * 8)
          "operator_code"
          (String operator_code)
      ; Node.make
          ~offset:(off + off_13)
          (length_4 * 8)
          "national_area_code"
          (String nat_area_code)
      ; Node.make
          ~offset:(off + off_14)
          (length_5 * 8)
          "core_number"
          (String core_number) ]
