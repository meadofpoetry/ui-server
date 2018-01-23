[@@@ocaml.warning "-32"]

[%%cstruct
 type prefix =
   { stx      : uint8_t
   ; address  : uint8_t [@len 2]
   ; category : uint8_t [@len 2]
   ; setting  : uint8_t [@len 2]
   ; rw       : uint8_t
   } [@@little_endian]]

[%%cstruct
 type suffix =
   { crc : uint8_t [@len 2]
   ; etx : uint8_t
   } [@@little_endian]]

[%%cstruct
 type setting8 =
   { data : uint8_t [@len 2]
   } [@@little_endian]]

[%%cstruct
 type setting16 =
   { data : uint8_t [@len 4]
   } [@@little_endian]]

[%%cstruct
 type setting32 =
   { data : uint8_t [@len 8]
   } [@@little_endian]]

[%%cstruct
 type setting48 =
   { data : uint8_t [@len 12]
   } [@@little_endian]]

[%%cstruct
 type setting64 =
   { data : uint8_t [@len 16]
   } [@@little_endian]]

[@@@ocaml.warning "+32"]
