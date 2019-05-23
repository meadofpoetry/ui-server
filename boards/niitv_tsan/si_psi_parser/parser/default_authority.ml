(* ETSI TS 102 323 *)

let name = "default_authority_descriptor"

let parse bs off =
  Bytes.parse ~offset:off bs "default_authority_byte"
