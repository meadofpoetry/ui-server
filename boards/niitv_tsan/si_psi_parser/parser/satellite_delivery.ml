let name = "satellite_delivery_system_descriptor"

let parse_polarization pol =
  match pol with
  | 0 -> "linear - horizontal"
  | 1 -> "linear - vertical"
  | 2 -> "Circular - left"
  | 3 -> "Circular - right"
  | _ -> assert false

let parse_rolloff rf =
  match rf with
  | 0 -> "a = 0,35"
  | 1 -> "a = 0,25"
  | 2 -> "a = 0,20"
  | 3 -> "reserved"
  | _ -> assert false

let parse_mod_system system =
  match system with false -> "DVB-S" | true -> "DVB-S2"

let parse_mod_type typ =
  match typ with
  | 0 -> "Auto"
  | 1 -> "QPSK"
  | 2 -> "8PSK"
  | 3 -> "16-QAM (n/a for DVB-S2)"
  | _ -> assert false

let parse bs off =
  match%bitstring bs with
  | {| frequency         : 32
     ; orbital_position  : 16 : save_offset_to (off_1)
     ; west_east_flag    : 1  : save_offset_to (off_2)
     ; polarization      : 2  : save_offset_to (off_3)
     ; roll_off          : 2  : save_offset_to (off_4)
     ; modulation_system : 1  : save_offset_to (off_5)
     ; modulation_type   : 2  : save_offset_to (off_6)
     ; symbol_rate       : 28 : save_offset_to (off_7)
     ; fec_inner         : 4  : save_offset_to (off_8)
     |}
    ->
      let pol = parse_polarization polarization in
      let rf = parse_rolloff roll_off in
      let mod_system = parse_mod_system modulation_system in
      let mod_type = parse_mod_type modulation_type in
      [
        Node.make ~offset:off 32 "frequency" (Dec (Int32 frequency));
        Node.make ~offset:(off + off_1) 16 "orbital_position"
          (Hex (Int orbital_position));
        Node.make ~offset:(off + off_2) 1 "west_east_flag"
          (Bits (Bool west_east_flag));
        Node.make ~parsed:pol ~offset:(off + off_3) 2 "polarization"
          (Bits (Int polarization));
        Node.make ~parsed:rf ~offset:(off + off_4) 2 "roll_off"
          (Bits (Int roll_off));
        Node.make ~parsed:mod_system ~offset:(off + off_5) 1 "modulation_system"
          (Bits (Bool modulation_system));
        Node.make ~parsed:mod_type ~offset:(off + off_6) 2 "modulation_type"
          (Bits (Int modulation_type));
        Node.make ~offset:(off + off_7) 28 "symbol_rate" (Dec (Int symbol_rate));
        Node.make ~offset:(off + off_8) 4 "FEC_inner" (Dec (Int fec_inner));
      ]
