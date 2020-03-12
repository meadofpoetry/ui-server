[@@@ocaml.warning "-32"]

type%cstruct prefix = {
  stx : uint8_t;
  address : uint8_t; [@len 2]
  category : uint8_t; [@len 2]
  setting : uint8_t; [@len 2]
  rw : uint8_t;
}
[@@little_endian]

type%cstruct suffix = { crc : uint8_t; [@len 2] etx : uint8_t }
[@@little_endian]

type%cstruct setting8 = { data : uint8_t [@len 2] } [@@little_endian]

type%cstruct setting16 = { data : uint8_t [@len 4] } [@@little_endian]

type%cstruct setting32 = { data : uint8_t [@len 8] } [@@little_endian]

type%cstruct setting48 = { data : uint8_t [@len 12] } [@@little_endian]

type%cstruct setting64 = { data : uint8_t [@len 16] } [@@little_endian]

[@@@ocaml.warning "+32"]

let stx = 0x02

let etx = 0x03
