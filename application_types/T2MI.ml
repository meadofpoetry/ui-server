module ETR290_error = struct
  type t =
    (* Measurements of the syntax of T2-MI packets *)
    | T2MI_packet_type_error_1
    | T2MI_packet_type_error_2
    | T2MI_packet_count_error
    | T2MI_CRC_error
    | T2MI_payload_error
    | T2MI_plp_num_blocks_error
    | T2MI_transmission_order_error
    | T2MI_DVB_T2_timestamp_error
    | T2MI_DVB_T2_timestamp_discontinuity
    | T2MI_T2_frame_length_error
    (* Checks on the T2-MI MIP (Modulator Information Packet) *)
    | T2MI_MIP_timestamp_error
    | T2MI_MIP_individual_addressing_error
    | T2MI_MIP_continuity_error
    | T2MI_MIP_CRC_error
    (* Check on consistency of T2-MI signalling information *)
    | T2MI_bandwidth_consistency_error
    | T2MI_DVB_T2_timestamp_leap_second_error
    | T2MI_DVB_T2_signalling_inconsistency_error

  let name : t -> string = function
    | T2MI_packet_type_error_1 -> "T2MI_packet_type_error_1"
    | T2MI_packet_type_error_2 -> "T2MI_packet_type_error_2"
    | T2MI_packet_count_error -> "T2MI_packet_count_error"
    | T2MI_CRC_error -> "T2MI_CRC_error"
    | T2MI_payload_error -> "T2MI_payload_error"
    | T2MI_plp_num_blocks_error -> "T2MI_plp_num_blocks_error"
    | T2MI_transmission_order_error -> "T2MI_transmission_order_error"
    | T2MI_DVB_T2_timestamp_error -> "T2MI_DVB-T2_Timestamp_error"
    | T2MI_DVB_T2_timestamp_discontinuity ->
        "T2MI_DVB-T2_Timestamp_discontinuity"
    | T2MI_T2_frame_length_error -> "T2MI_T2_frame_length_error"
    | T2MI_MIP_timestamp_error -> "T2MI_MIP_timestamp_error"
    | T2MI_MIP_individual_addressing_error ->
        "T2MI_MIP_individual_addressing_error"
    | T2MI_MIP_continuity_error -> "T2MI_MIP_continuity_error"
    | T2MI_MIP_CRC_error -> "T2MI_MIP_CRC_error"
    | T2MI_bandwidth_consistency_error -> "T2MI_bandwidth_consistency_error"
    | T2MI_DVB_T2_timestamp_leap_second_error ->
        "T2MI_DVB-T2_Timestamp_leap_second_error"
    | T2MI_DVB_T2_signalling_inconsistency_error ->
        "T2MI_DVB_T2_signalling_inconsistency_error"

  let number : t -> string = function
    | T2MI_packet_type_error_1 -> "11.2.2.1"
    | T2MI_packet_type_error_2 -> "11.2.2.2"
    | T2MI_packet_count_error -> "11.2.2.3"
    | T2MI_CRC_error -> "11.2.2.4"
    | T2MI_payload_error -> "11.2.2.5"
    | T2MI_plp_num_blocks_error -> "11.2.2.6"
    | T2MI_transmission_order_error -> "11.2.2.7"
    | T2MI_DVB_T2_timestamp_error -> "11.2.2.8"
    | T2MI_DVB_T2_timestamp_discontinuity -> "11.2.2.9"
    | T2MI_T2_frame_length_error -> "11.2.2.10"
    | T2MI_MIP_timestamp_error -> "11.2.3.1"
    | T2MI_MIP_individual_addressing_error -> "11.2.3.2"
    | T2MI_MIP_continuity_error -> "11.2.3.3"
    | T2MI_MIP_CRC_error -> "11.2.3.4"
    | T2MI_bandwidth_consistency_error -> "11.2.4.1"
    | T2MI_DVB_T2_timestamp_leap_second_error -> "11.2.4.2"
    | T2MI_DVB_T2_signalling_inconsistency_error -> "11.2.4.3"
end
