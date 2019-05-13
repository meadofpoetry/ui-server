(* refers to ETSI TS 102 812 *)

let name = "service_identifier_descriptor"

let parse bs off =
  Bytes.parse ~offset:off bs "textual_service_identifier_bytes"
