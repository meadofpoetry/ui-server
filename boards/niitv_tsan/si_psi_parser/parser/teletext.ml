open Util

let name = "teletext_descriptor"

let parse_type = function
  | 0x00 -> "reserved for future use"
  | 0x01 -> "initial Teletext page"
  | 0x02 -> "Teletext subtitle page"
  | 0x03 -> "additional information page"
  | 0x04 -> "programme schedule page"
  | 0x05 -> "Teletext subtitle page for hearing impaired people"
  | x when x > 0x05 && x < 0x20 -> "reserved_for_future_use"
  | x -> Printf.sprintf "%d" x

let rec parse bs off =
  if Bitstring.length bs = 0 then []
  else match%bitstring bs with
    | {| lang_code : 24 : bitstring
       ; txt_type  : 5  : save_offset_to (off_1)
       ; mag_num   : 3  : save_offset_to (off_2)
       ; page_num  : 8  : save_offset_to (off_3)
       ; rest      : -1 : save_offset_to (off_4), bitstring
       |} ->
      let p_code, lang_code = Language_code.parse lang_code in
      let parsed = parse_type txt_type in
      let nodes =
        [ Node.make ~parsed:p_code ~offset:off 24 "ISO_639_language_code" (Bits (Int lang_code))
        ; Node.make ~parsed ~offset:(off + off_1) 5 "teletext_type" (Hex(Int txt_type))
        ; Node.make ~offset:(off + off_2) 3 "teletext_magazine_number" (Dec (Int mag_num))
        ; Node.make ~offset:(off + off_3) 8 "teletext_page_number" (Dec (Int page_num)) ]
      in
      let node = Node.make ~offset:off 40 p_code (List nodes) in
      node :: parse rest (off + off_4)
