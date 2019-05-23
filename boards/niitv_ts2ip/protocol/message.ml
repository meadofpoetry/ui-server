let n_udp = 34
let n_udp_main = 10
let n_udp_aux = 12

[@@@ocaml.warning "-32"]

[%%cstruct
  type prefix =
    { tag_start : uint16_t
    ; msg_code : uint16_t
    } [@@little_endian]]

[%%cstruct
  type rsp_devinfo =
    { typ : uint8_t
    ; ver : uint8_t
    ; packers_num : uint8_t
    ; rfu : uint8_t
    } [@@little_endian]]

[%%cstruct
  type udp_settings =
    { self_port : uint16_t
    ; dst_mac : uint16_t [@len 3]
    ; dst_ip : uint32_t
    ; dst_port : uint16_t
    ; stream_id : uint32_t
    ; mode : uint16_t
    } [@@little_endian]]

[%%cstruct
  type req_mode_main_prefix =
    { cmd : uint16_t
    ; ip : uint32_t
    ; mask : uint32_t
    ; gateway : uint32_t
    ; rfu : uint16_t [@len 17]
    } [@@little_endian]]

[%%cstruct
  type req_mode_main_suffix =
    { rfu : uint16_t [@len 4]
    } [@@little_endian]]

[%%cstruct
  type req_mode_aux_prefix =
    { cmd : uint16_t
    ; rfu : uint16_t [@len 3]
    } [@@little_endian]]

[%%cstruct
  type req_mode_aux_suffix =
    { rfu : uint16_t [@len 4]
    } [@@little_endian]]

[%%cstruct
  type req_factory_mode =
    { mac : uint16_t [@len 3]
    ; rfu : uint16_t [@len 125]
    } [@@little_endian]]

[%%cstruct
  type udp_status =
    { rate : uint32_t
    ; stream : uint32_t
    } [@@little_endian]]

[%%cstruct
  type status =
    { rfu : uint16_t
    ; phy : uint16_t
    ; input : uint16_t
    ; rfu_2 : uint16_t
    ; data : uint16_t [@len 124]
    } [@@little_endian]]

[@@@ocaml.warning "+32"]

let tag_start = 0x55AA
