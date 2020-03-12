let name = "system_clock_descriptor"

let external_clock_ref_indicator_of_bool ecri =
  if ecri then "system clock has derived" else "system clock has not derived"

let parse bs off =
  match%bitstring bs with
  | {| ext_clock_ref_ind : 1
     ; reserved_1        : 1 : save_offset_to (off_1)
     ; clock_acc_int     : 6 : save_offset_to (off_2)
     ; clock_acc_exp     : 3 : save_offset_to (off_3)
     ; reserved_2        : 5 : save_offset_to (off_4)
     |}
    ->
      [
        Node.make ~offset:off 1 "external_clock_reference_indicator"
          (Bits (Bool ext_clock_ref_ind));
        Node.make ~offset:(off_1 + off) 1 "reserved" (Bits (Bool reserved_1));
        Node.make ~offset:(off_2 + off) 6 "clock_accuracy_integer"
          (Dec (Int clock_acc_int));
        Node.make ~offset:(off_3 + off) 3 "clock_accuracy_exponent"
          (Dec (Int clock_acc_exp));
        Node.make ~offset:(off_4 + off) 5 "reserved" (Bits (Int reserved_2));
      ]
