let name = "MVC extension descriptor"

let parse bs off =
  match%bitstring bs with
  | {| average_bit_rate  : 16
     ; maximum_bitrate   : 16 : save_offset_to (off_1)
     ; assoc_not_pr      : 1  : save_offset_to (off_2)
     ; is_left_eyeview   : 1  : save_offset_to (off_3)
     ; reserved          : 2  : save_offset_to (off_4)
     ; order_index_min   : 10 : save_offset_to (off_5)
     ; order_index_max   : 10 : save_offset_to (off_6)
     ; temporal_id_start : 3  : save_offset_to (off_7)
     ; temporal_id_end   : 3  : save_offset_to (off_8)
     ; nsnu_present      : 1  : save_offset_to (off_9)
     ; npnu_present      : 1  : save_offset_to (off_10)
     |} ->
    [ Node.make ~offset:off 16 "average_bit_rate" (Dec (Int average_bit_rate))
    ; Node.make ~offset:(off + off_1)  16 "maximum_bitrate" (Dec (Int maximum_bitrate))
    ; Node.make ~offset:(off + off_2)  1  "view_association_not_present" (Bits (Bool assoc_not_pr))
    ; Node.make ~offset:(off + off_3)  1  "base_view_is_left_eyeview" (Bits (Bool is_left_eyeview))
    ; Node.make ~offset:(off + off_4)  2  "reserved" (Bits (Int reserved))
    ; Node.make ~offset:(off + off_5)  10 "view_order_index_min" (Dec (Int order_index_min))
    ; Node.make ~offset:(off + off_6)  10 "view_order_index_max" (Dec (Int order_index_max))
    ; Node.make ~offset:(off + off_7)  3  "temporal_id_start" (Hex (Int temporal_id_start))
    ; Node.make ~offset:(off + off_8)  3  "temporal_id_end" (Hex (Int temporal_id_end))
    ; Node.make ~offset:(off + off_9)  1  "no_sei_nal_unit_present" (Bits (Bool nsnu_present))
    ; Node.make ~offset:(off + off_10) 1  "no_prefix_nal_unit_present" (Bits (Bool npnu_present))
    ]
