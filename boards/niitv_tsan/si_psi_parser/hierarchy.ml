let name = "hierarchy_descriptor"

let hierarchy_to_string = function
  | 1  -> "Spatial_scalability"
  | 2  -> "SNR_scalability"
  | 3  -> "Temporal_scalability"
  | 4  -> "Data_partitioning"
  | 5  -> "Extension_bitstream"
  | 6  -> "Private_stream"
  | 7  -> "Multi_view_profile"
  | 8  -> "Combined_scalability"
  | 9  -> "MVC_or_MVCD"
  | 15 -> "Base_layer_etc"
  | _  -> "Reserved"

let parse bs off =
  match%bitstring bs with
  | {| reserved_1           : 1
     ; temporal_scalability : 1 : save_offset_to (off_1)
     ; spatial_scalability  : 1 : save_offset_to (off_2)
     ; quality_scalability  : 1 : save_offset_to (off_3)
     ; hier_typ             : 4 : save_offset_to (off_4)
     ; reserved_2           : 2 : save_offset_to (off_5)
     ; hierarchy_index      : 6 : save_offset_to (off_6)
     ; tref_present_flag    : 1 : save_offset_to (off_7)
     ; reserved_3           : 1 : save_offset_to (off_8)
     ; emb_index            : 6 : save_offset_to (off_9)
     ; reserved_4           : 2 : save_offset_to (off_10)
     ; hierarchy_channel    : 6 : save_offset_to (off_11)
     |} ->
    let parsed = hierarchy_to_string hier_typ in
    [ Node.make ~offset:off 1 "reserved_1" (Bits (Bool reserved_1))
    ; Node.make ~offset:(off_1 + off)  1 "temporal_scalability" (Bits (Bool temporal_scalability))
    ; Node.make ~offset:(off_2 + off)  1 "quality_scalability" (Bits (Bool quality_scalability))
    ; Node.make ~offset:(off_3 + off)  1 "spatial_scalability" (Bits (Bool spatial_scalability))
    ; Node.make ~parsed ~offset:(off_4 + off) 4 "hierarchy_type" (Hex (Int hier_typ))
    ; Node.make ~offset:(off_5 + off)  2 "reserved_2" (Bits (Int reserved_2))
    ; Node.make ~offset:(off_6 + off)  6 "hierarchy_layer_index" (Dec (Int hierarchy_index))
    ; Node.make ~offset:(off_7 + off)  1 "tref_present_flag" (Bits (Bool tref_present_flag))
    ; Node.make ~offset:(off_8 + off)  1 "reserved_3" (Bits (Bool reserved_3))
    ; Node.make ~offset:(off_9 + off)  6 "hierarchy_embedded_layer_index" (Dec (Int emb_index))
    ; Node.make ~offset:(off_10 + off) 2 "reserved_4" (Bits (Int reserved_4))
    ; Node.make ~offset:(off_11 + off) 6 "hierarchy_channel" (Dec (Int hierarchy_channel))
    ]
