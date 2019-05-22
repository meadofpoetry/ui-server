let name = "Extension_descriptor"

let parse_tag = function
  | 0 -> "Reserved"
  | 1 -> "Forbidden"
  | 2 -> "ODUpdata_descriptor"
  | 3 -> "HEVC_timing_and_HRD_descriptor"
  | x when x >= 4 && x <= 255 -> "Rec. ITU-T H.222.0 | ISO/IEC 13818-1 Reserved"
  | _ -> assert false

let parse bs off =
  match%bitstring bs with
  | {| 0x02 :  8
     ; rest : -1 : save_offset_to (off_1), bitstring
     |} ->
    (* NOTE refers to 8.5.5.2 of ISO/IEC 14496-1, doesn't exist *)
    let str = Bitstring.string_of_bitstring rest in
    let nodes =
      [ Node.make ~parsed:(parse_tag 2) ~offset:off 8 "extension_descriptor_tag" (Hex (Int 0x02))
      ; Node.make ~parsed:str ~offset:(off + off_1)
          (Bitstring.bitstring_length rest)
          "rest" (Bits (Bool false))]
    in
    nodes @ []
  (* FIXME *)
  | {| 0x03 : 8
     ; rest : -1 : save_offset_to (off_1), bitstring
     |} ->
    let node =
      [ Node.make ~parsed:(parse_tag 3) ~offset:off 8 "extension_descriptor_tag" (Hex (Int 0x03))]
    in
    node @ (Hevc_hrd.decode (off + off_1) rest)
  | {| ext_desc_tag : 8
     ; rest         : -1 : save_offset_to (off_1), bitstring
     |} ->
    let node = Node.make ~parsed:(parse_tag ext_desc_tag) ~offset:off
        8 "extension_descriptor_tag" (Hex (Int ext_desc_tag)) in
    node :: Bytes.parse ~offset:(off + off_1) rest "reserved"
