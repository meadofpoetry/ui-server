let name = "short_event_descriptor"

(* TODO  is this supposed to work like it does
 *  let parse_lang_code code =
 *   match%bitstring code with
 *   | {| ch_1 : 8 : string
 *      ; ch_2 : 8 : string
 *      ; ch_3 : 8 : string
 *      |} ->
 *      Printf.sprintf "%s%s%s" ch_1 ch_2 ch_3 *)

let parse bs off =
  match%bitstring bs with
  | {| lang_code  : 24 : bitstring
     ; length_1   : 8  : save_offset_to (off_1)
     ; event_name : length_1 * 8 : save_offset_to (off_2), bitstring
     ; length_2   : 8  : save_offset_to (off_3)
     ; text       : length_2 * 8 : save_offset_to (off_4), bitstring
     |} ->
    let name =
      match Text_decoder.decode @@ Util.Bitstring.to_cstruct event_name with
      | Ok s -> s
      | Error _ -> "Failed to decode" in
    let text =
      match Text_decoder.decode @@ Util.Bitstring.to_cstruct text with
      | Ok s -> s
      | Error _ -> "Failed to decode" in
    let parsed_code, lang_code = Language_code.parse lang_code in
    [ Node.make ~parsed:parsed_code ~offset:off 24 "ISO_639_language_code" (Bits (Int lang_code))
    ; Node.make ~offset:(off + off_1) 8 "event_name_length" (Dec (Int length_1))
    ; Node.make ~offset:(off + off_2) (length_1 * 8) "event_name" (String name)
    ; Node.make ~offset:(off + off_3) 8 "text_length" (Dec (Int length_2))
    ; Node.make ~offset:(off + off_4) (length_2 * 8) "text" (String text)
    ]
