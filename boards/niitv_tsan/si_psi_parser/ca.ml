let name = "CA_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| ca_system_id : 16
     ; reserved     : 3  : save_offset_to (off_1)
     ; ca_pid       : 13 : save_offset_to (off_2)
     ; private_data : -1 : save_offset_to (off_3), bitstring
     |} ->
    let nodes =
      [ Node.make ~offset:off 16 "CA_system_id" (Hex (Int ca_system_id))
      ; Node.make ~offset:(off_1 + off) 3  "reserved" (Bits (Int reserved))
      ; Node.make ~offset:(off_2 + off) 13 "CA_PID" (Hex (Int ca_pid)) ]
    in
    nodes @ Bytes.parse ~offset:(off + off_3) private_data "private_data_byte"
