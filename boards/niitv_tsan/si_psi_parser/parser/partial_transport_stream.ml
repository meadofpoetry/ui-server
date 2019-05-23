let name = "Partial Transport Stream (TS) descriptor"

let parse bs off =
  match%bitstring bs with
  | {| dvb_rfu_1 : 2
     ; peak_rate : 22 : save_offset_to (off_1)
     ; dvb_rfu_2 : 2  : save_offset_to (off_2)
     ; min_rate  : 22 : save_offset_to (off_3)
     ; dvb_rfu_3 : 2  : save_offset_to (off_4)
     ; max_rate  : 14 : save_offset_to (off_5)
     |} ->
    [ Node.make ~offset:off 2 "DVB_reserved_future_use" (Bits (Int dvb_rfu_1))
    ; Node.make ~offset:(off + off_1) 22 "peak_rate" (Dec (Uint peak_rate))
    ; Node.make ~offset:(off + off_2) 2  "DVB_reserved_future_use" (Bits (Int dvb_rfu_2))
    ; Node.make ~offset:(off + off_3) 22 "minimum_overall_smoothing_rate" (Dec (Uint min_rate))
    ; Node.make ~offset:(off + off_4) 2  "DVB_reserved_future_use" (Bits (Int dvb_rfu_3))
    ; Node.make ~offset:(off + off_5) 14 "maximum_overall_smoothing_rate" (Dec (Uint max_rate))
    ]
