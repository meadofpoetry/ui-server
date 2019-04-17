[@@@ocaml.warning "-32"]

[%%cstruct
  type prefix =
    { tag_start : uint16_t
    ; msg_code : uint16_t
    } [@@little_endian]]

[%%cstruct
  type board_info =
    { typ : uint8_t
    ; ver : uint8_t
    ; packers_num : uint8_t
    ; rfu : uint8_t
    } [@@little_endian]]

[%%cstruct
  type factory_settings =
    { mac : uint16_t [@len 3]
    ; rfu : uint16_t [@len 61]
    } [@@little_endian]]

[%%cstruct
  type packer_settings =
    { self_port : uint16_t
    ; dst_mac : uint8_t [@len 6]
    ; dst_ip : uint32_t
    ; dst_port : uint16_t
    ; stream_id : uint32_t
    ; mode : uint8_t
    ; rfu : uint8_t
    } [@@little_endian]]

[%%cstruct
  type req_settings_main =
    { cmd : uint16_t
    ; ip : uint32_t
    ; mask : uint32_t
    ; gateway : uint32_t
    ; rfu : uint16_t [@len 17]
    } [@@little_endian]]

[%%cstruct
  type req_settings_packers =
    { cmd : uint16_t
    ; rfu : uint16_t [@len 3]
    } [@@little_endian]]

[%%cstruct
  type status =
    { sub_cmd : uint16_t
    ; phy : uint16_t
    ; rfu : uint32_t
    ; data : uint32_t [@len 30]
    } [@@little_endian]]

[@@@ocaml.warning "+32"]

let tag_start = 0x55AA
