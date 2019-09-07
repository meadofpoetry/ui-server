let name = "cable_delivery_system_descriptor"

let parse_outer_fec outer =
  match outer with
  | 0 -> "not defined"
  | 1 -> "not outer FEC coding"
  | 2 -> "RS(204/188)"
  | _ -> "reserved for future use"

let parse_modulation_scheme modulation =
  match modulation with
  | 0x00 -> "not defined"
  | 0x01 -> "16-QAM"
  | 0x02 -> "32-QAM"
  | 0x03 -> "64-QAM"
  | 0x04 -> "128-QAM"
  | 0x05 -> "256-QAM"
  | _ -> "reserved for future use"

let parse_inner_fec inner =
  match inner with
  | 0 -> "defined"
  | 1 -> "1/2 conv. code rate"
  | 2 -> "2/3 conv. code rate"
  | 3 -> "3/4 conv. code rate"
  | 4 -> "5/6 conv. code rate"
  | 5 -> "7/8 conv. code rate"
  | 6 -> "8/9 conv. code rate"
  | 7 -> "3/5 conv. code rate"
  | 8 -> "4/5 conv. code rate"
  | 9 -> "9/10 conv. code rate"
  | 15 -> "no conv. Coding"
  | _ -> "reserved for future use"

let parse bs off =
  match%bitstring bs with
  | {| frequency   : 32
     ; rfu         : 12 : save_offset_to (off_1)
     ; fec_outer   : 4  : save_offset_to (off_2)
     ; modulation  : 8  : save_offset_to (off_3)
     ; symbol_rate : 28 : save_offset_to (off_4)
     ; fec_inner   : 4  : save_offset_to (off_5)
     |}
    ->
      let parsed_mod = parse_modulation_scheme modulation in
      let parsed_out = parse_outer_fec fec_outer in
      let parsed_in = parse_inner_fec fec_inner in
      [ Node.make ~offset:off 32 "frequency" (Dec (Int32 frequency))
      ; Node.make ~offset:(off + off_1) 12 "reserved_future_use" (Bits (Int rfu))
      ; Node.make ~parsed:parsed_out ~offset:(off + off_2) 4 "FEC_outer" (Dec (Int rfu))
      ; Node.make ~parsed:parsed_mod ~offset:(off + off_3) 8 "modulation" (Hex (Int rfu))
      ; Node.make ~offset:(off + off_4) 28 "symbol_rate" (Dec (Int symbol_rate))
      ; Node.make ~parsed:parsed_in ~offset:(off + off_5) 4 "FEC_inner" (Dec (Int rfu))
      ]
