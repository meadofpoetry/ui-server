(* ETSI EN 301 192 *)

let name = "time_slice_fec_identifier_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| time_slicing      : 1
     ; mpe_fec           : 2  : save_offset_to (off_1)
     ; rfu               : 2  : save_offset_to (off_2)
     ; frame_size        : 3  : save_offset_to (off_3)
     ; max_burst_dur     : 8  : save_offset_to (off_4)
     ; max_aver_rate     : 4  : save_offset_to (off_5)
     ; time_slice_fec_id : 4  : save_offset_to (off_6)
     ; rest              : -1 : save_offset_to (off_7), bitstring
     |} ->
    let id_selector =
      match Text_decoder.decode @@ Util.Bitstring.to_cstruct rest with
      | Ok s -> s
      | Error _ -> "Unable to decode" in
    [ Node.make ~offset:off 1 "time_slicing" (Bits (Bool time_slicing))
    ; Node.make ~offset:(off + off_1) 2 "mpe_fec" (Dec (Int mpe_fec))
    ; Node.make ~offset:(off + off_2) 2 "reserved_for_future_use" (Bits (Int rfu))
    ; Node.make ~offset:(off + off_3) 3 "frame_size" (Dec (Int frame_size))
    ; Node.make ~offset:(off + off_4) 8 "max_burst_duration" (Dec (Int max_burst_dur))
    ; Node.make ~offset:(off + off_5) 4 "max_average_rate" (Dec (Int max_aver_rate))
    ; Node.make ~offset:(off + off_6) 4 "time_slice_fec_id" (Dec (Int time_slice_fec_id))
    ; Node.make ~offset:(off + off_7)
        (Bitstring.bitstring_length rest) "id_selector" (String id_selector)
    ]
