let tag_start = 0x55AA
let tag_stop  = 0xFE

[@@@ocaml.warning "-32"]

[%%cstruct
  type prefix =
    { tag_start : uint16_t
    ; length : uint8_t
    ; msg_code : uint8_t
    } [@@little_endian]]

[%%cstruct
  type suffix =
    { crc : uint8_t
    ; tag_stop : uint8_t
    } [@@little_endian]]

[%%cstruct
  type cmd_src_id =
    { source_id : uint8
    } [@@little_endian]]

[%%cstruct
  type rsp_src_id =
    { source_id : uint8
    ; rfu : uint8
    } [@@little_endian]]

[%%cstruct
  type cmd_devinfo =
    { reset : uint8_t
    } [@@little_endian]]

[%%cstruct
  type rsp_devinfo =
    { serial : uint16_t
    ; hw_ver : uint8_t
    ; fpga_ver : uint8_t
    ; soft_ver : uint8_t
    ; hw_config : uint8_t
    ; rfu : uint32_t
    } [@@little_endian]]

[%%cstruct
  type mode =
    { standard : uint8_t
    ; bw : uint8_t
    ; hw_present : uint8_t  (* rfu in cmd *)
    ; rfu : uint8_t  (* former dvb-c qam *)
    ; freq : uint32_t
    ; plp : uint8_t
    ; lock : uint8_t  (* rfu in cmd *)
    } [@@little_endian]]

[%%cstruct
  type rsp_measure =
    { lock : uint8_t
    ; power : uint16_t
    ; mer : uint16_t
    ; ber : uint32_t
    ; freq : uint32_t
    ; bitrate : uint32_t
    } [@@little_endian]]

[%%cstruct
  type rsp_params =
    { lock : uint8_t
    ; fft : uint8_t
    ; gi : uint8_t
    ; bw_ext : uint8_t
    ; papr : uint8_t
    ; l1_rep : uint8_t
    ; l1_mod : uint8_t
    ; freq : uint32_t
    ; l1_post_sz : uint16_t
    ; l1_post_info_sz : uint16_t
    ; tr_fmt : uint8_t
    ; sys_id : uint16_t
    ; net_id : uint16_t
    ; cell_id : uint16_t
    ; t2_frames : uint8_t
    ; ofdm_syms : uint16_t
    ; pp : uint8_t
    ; plp_num : uint8_t
    ; tx_id_avail : uint8_t
    ; num_rf : uint8_t
    ; cur_rf_id : uint8_t
    ; rfu : uint8_t
    ; cur_plp_id : uint8_t
    ; plp_type : uint8_t
    ; cr : uint8_t
    ; plp_mod : uint8_t
    ; rotation : uint8_t
    ; fec_size : uint8_t
    ; fec_block_num : uint16_t
    ; in_band_flag : uint8_t
    } [@@little_endian]]

[%%cstruct
  type rsp_plp_list =
    { lock : uint8_t
    ; plp_qty : uint8_t
    } [@@little_endian]
]

[%%cstruct
  type cmd_plp_set =
    { plp_id : uint8_t
    } [@@little_endian]]

[%%cstruct
  type rsp_plp_set =
    { lock : uint8_t
    ; plp : uint8_t
    } [@@little_endian]]

[@@@ocaml.warning "+32"]

let bool_of_int = function
  | 0xFF -> Some true
  | 0x00 -> Some false
  | _ -> None
