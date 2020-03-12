let name = "scrambling_descriptor"

let parse_mode mode =
  match mode with
  | 0x01 -> "DVB-CSA1"
  | 0x02 -> "DVB-CSA2"
  | 0x03 -> "DVB-CSA3 in standard mode"
  | 0x04 -> "DVB-CSA3 in minimally enhanced mode"
  | 0x05 -> "DVB-CSA3 in fully enhanced mode"
  | 0x10 -> "DVB-CISSA version 1"
  | 0x00 | 0xFF -> "Reserved for future use"
  | x when (x > 0x05 && x < 0x10) || (x > 0x1F && x < 0x70) ->
      "Reserved for future use"
  | x when x > 0x10 && x < 0x20 -> "Reserved for future DVB-CSIISA versions"
  | x when x > 0x6F && x < 0x80 -> "ATIS defined (ATIS-0800006, see annex J)"
  | x when x > 0x80 && x < 0xFF -> "User defined"
  | _ -> assert false

let parse bs off =
  match%bitstring bs with
  | {| scrambling_mode : 8 |} ->
      let parsed = parse_mode scrambling_mode in
      [
        Node.make ~parsed ~offset:off 32 "scrambling_mode"
          (Hex (Int scrambling_mode));
      ]
