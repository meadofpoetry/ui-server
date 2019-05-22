let name = "data_broadcast_id_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| data_broadcast_id : 16
     ; rest              : -1 : save_offset_to (off_1), bitstring
     |} ->
    let node = Node.make ~offset:off 16 "data_broadcast_id" (Hex (Int data_broadcast_id)) in
    node :: Bytes.parse ~offset:(off + off_1) rest "id_selector_byte"
