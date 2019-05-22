let name = "Extension descriptor"

let parse bs off =
  match%bitstring bs with
  | {| desc_tag_ext : 8
     ; rest         : -1 : save_offset_to (off_1), bitstring
     |} ->
    let node = Node.make ~offset:off 8 "descriptor_tag_extension" (Hex (Int desc_tag_ext)) in
    node :: Bytes.parse ~offset:(off + off_1) rest "selector_byte"
