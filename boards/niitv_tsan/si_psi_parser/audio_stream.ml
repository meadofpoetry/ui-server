let name = "audio_stream_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| free_format_flag : 1
     ; id               : 1 : save_offset_to (off_1)
     ; layer            : 2 : save_offset_to (off_2)
     ; variable_rate    : 1 : save_offset_to (off_3)
     ; reserved         : 3 : save_offset_to (off_4)
     |} ->
    [ Node.make ~offset:off 1 "free_format_flag" (Bits (Bool free_format_flag))
    ; Node.make ~offset:(off + off_1) 1 "id" (Bits (Bool id))
    ; Node.make ~offset:(off + off_2) 2 "layer" (Dec (Int layer))
    ; Node.make ~offset:(off + off_3) 1 "variable_rate" (Bits (Bool variable_rate))
    ; Node.make ~offset:(off + off_4) 3 "reserved" (Bits (Int reserved)) ]
