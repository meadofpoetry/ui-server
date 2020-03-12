let name = "terrestrial_delivery_system_descriptor"

let parse_bandwith bandwith =
  match bandwith with
  | 0 -> "8 MHz"
  | 1 -> "7 MHz"
  | 2 -> "6 MHz"
  | 3 -> "5 MHz"
  | _ -> "Reserved for future use"

let parse_priority priority =
  match priority with
  | true -> "HP (high priority)"
  | false -> "LP (low priority)"

let parse_constellation const =
  match const with
  | 0 -> "QPSK"
  | 1 -> "16-QAM"
  | 2 -> "64-QAM"
  | 3 -> "reserved for future use"
  | _ -> assert false

let parse_hierarchy hier =
  match hier with
  | 0 -> "non-hierarchical, native interleaver"
  | 1 -> "a = 1, native interleaver"
  | 2 -> "a = 2, native interleaver"
  | 3 -> "a = 4, native interleaver"
  | 4 -> "non-hierarchical, in-depth interleaver"
  | 5 -> "a = 1, in-depth interleaver"
  | 6 -> "a = 2, in-depth interleaver"
  | 7 -> "a = 4, in-depth interleaver"
  | _ -> assert false

let parse_code_rate cr =
  match cr with
  | 0 -> "1/2"
  | 1 -> "2/3"
  | 2 -> "3/4"
  | 3 -> "5/6"
  | 4 -> "7/8"
  | _ -> "reserved_for_future_use"

let parse_guard_interval gi =
  match gi with
  | 0 -> "1/32"
  | 1 -> "1/16"
  | 2 -> "1/8"
  | 3 -> "1/4"
  | _ -> assert false

let parse_transmission tr =
  match tr with
  | 0 -> "2k mode"
  | 1 -> "8k mode"
  | 2 -> "4k mode"
  | 3 -> "reserved for future use"
  | _ -> assert false

let parse bs off =
  match%bitstring bs with
  | {| centre_frequency  : 32
     ; bandwith          : 3  : save_offset_to (off_1)
     ; priority          : 1  : save_offset_to (off_2)
     ; time_slicing_ind  : 1  : save_offset_to (off_3)
     ; mpe_fec_ind       : 1  : save_offset_to (off_4)
     ; rfu_1             : 2  : save_offset_to (off_5)
     ; constellation     : 2  : save_offset_to (off_6)
     ; hierarchy_info    : 3  : save_offset_to (off_7)
     ; code_rate_hp_str  : 3  : save_offset_to (off_8)
     ; code_rate_lp_str  : 3  : save_offset_to (off_9)
     ; guard_interval    : 2  : save_offset_to (off_10)
     ; transmission_mode : 2  : save_offset_to (off_11)
     ; other_freq_flag   : 1  : save_offset_to (off_12)
     ; rfu_2             : 32 : save_offset_to (off_13)
     |}
    ->
      let trans = parse_transmission transmission_mode in
      let guard = parse_guard_interval guard_interval in
      let hp_cr = parse_code_rate code_rate_hp_str in
      let lp_cr = parse_code_rate code_rate_lp_str in
      let hier = parse_hierarchy hierarchy_info in
      let const = parse_constellation constellation in
      let prior = parse_priority priority in
      let bandw = parse_bandwith bandwith in
      [
        Node.make ~offset:off 32 "centre_frequency"
          (Dec (Int32 centre_frequency));
        Node.make ~parsed:bandw ~offset:(off + off_1) 3 "bandwith"
          (Dec (Int bandwith));
        Node.make ~parsed:prior ~offset:(off + off_2) 1 "priority"
          (Bits (Bool priority));
        Node.make ~offset:(off + off_3) 1 "Time_slicing_indicator"
          (Bits (Bool time_slicing_ind));
        Node.make ~offset:(off + off_4) 1 "MPE-FEC-indicator"
          (Bits (Bool mpe_fec_ind));
        Node.make ~offset:(off + off_5) 2 "reserved_future_use"
          (Bits (Int rfu_1));
        Node.make ~parsed:const ~offset:(off + off_6) 2 "constellation"
          (Bits (Int rfu_1));
        Node.make ~parsed:hier ~offset:(off + off_7) 3 "hierarchy_information"
          (Hex (Int hierarchy_info));
        Node.make ~parsed:hp_cr ~offset:(off + off_8) 3 "code_rate-HP_stream"
          (Dec (Int code_rate_hp_str));
        Node.make ~parsed:lp_cr ~offset:(off + off_9) 3 "code_rate-LP_stream"
          (Dec (Int code_rate_lp_str));
        Node.make ~parsed:guard ~offset:(off + off_10) 2 "guard_interval"
          (Dec (Int guard_interval));
        Node.make ~parsed:trans ~offset:(off + off_11) 2 "transmission_mode"
          (Dec (Int transmission_mode));
        Node.make ~offset:(off + off_12) 1 "other_frequency_flag"
          (Bits (Bool other_freq_flag));
        Node.make ~offset:(off + off_13) 32 "reserved_future_use"
          (Bits (Int32 rfu_2));
      ]
