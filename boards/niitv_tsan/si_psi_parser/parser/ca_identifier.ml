let name = "CA_identifier_descriptor"

let parse_system_id = function
  | 0x0000 -> "Reserved"
  | x when x > 0x0000 && x < 0x0100 -> "Reserved for registration to standardized \
                                        systems through the DVB Project Office"
  | x when x > 0x00FF && x <= 0xFFFF -> "Reserved for general registration through \
                                         the DVB Project Office"
  | _ -> assert false

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0 then []
  else match%bitstring bs with
    | {| ca_sys_id : 16
       ; rest      : -1 : save_offset_to (off_1), bitstring
       |} ->
      let parsed = parse_system_id ca_sys_id in
      let node = Node.make ~parsed ~offset:off 16 "CA_system_id" (Hex (Int ca_sys_id)) in
      node :: parse rest (off + off_1)
