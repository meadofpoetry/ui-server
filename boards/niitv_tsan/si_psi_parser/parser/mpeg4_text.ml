let name = "MPEG-4_text_descriptor"

(* FIXME *)
(* defined in ITU-T Rec.H.220.0 *)
(* and refers to ISO/IEC 14496-17 TextConfig 5.3 page 3 *)
(* and to TS 26.245 page 12 then*)

let parse_base_format = function
  | 0x10 -> "Timed Text as specified in 3GPP TS 26.245"
  | x when (x >= 0x00 && x <= 0x0F) || (x >= 0x11 && x <= 0xFF) -> "Reserved"
  | _ -> assert false

let parse_profile_level = function
  | 0x10 -> "Base profile, base level"
  | x when (x >= 0x00 && x <= 0x0F) || (x >= 0x11 && x <= 0xFF) -> "Reserved"
  | _ -> assert false

let parse_descr_flags = function
  | 0x00 -> "Forbidden"
  | 0x01 -> "No in-band, out-of band only"
  | 0x10 -> "In-band onlym no out-of band"
  | 0x11 -> "Both in-band and out-of band"
  | _ -> assert false

let parse_text_format = function
  | 0x00 | 0xFF -> "Reserved"
  | 0x01 -> "Timed Text as specified in 3GPP TS 26.245"
  | x when x >= 0x02 && x <= 0xEF -> "Reserved"
  | x when x >= 0xF0 && x <= 0xFE -> "User-private"
  | _ -> assert false

(*    let decode_specific_text_config bs off =
      let len = Bitstring.length bs - 184 in
      match%bitstring bs with
      | {| base_format    : 8
         ; profile_level  : 8   : save_offset_to (off_1)
         ; duration_clock : 24  : save_offset_to (off_2)
         ; true           : 1   : save_offset_to (off_3)
         ; descr_flags    : 2   : save_offset_to (off_4)
         ; true           : 1   : save_offset_to (off_5)
         ; true           : 1   : save_offset_to (off_6)
         ; reserved       : 3   : save_offset_to (off_7)
         ; layer          : 8   : save_offset_to (off_8)
         ; text_track_w   : 16  : save_offset_to (off_9)
         ; text_track_h   : 16  : save_offset_to (off_10)
         ; num_of_formats : 8   : save_offset_to (off_11)
         ; comp_format    : 8   : save_offset_to (off_12)
         ; num_sam_descr  : 8   : save_offset_to (off_13)
         ; sample_index   : 8   : save_offset_to (off_14)
         ; sample_descr   : len : save_offset_to (off_15) idk how to parse dis line
         ; scene_width    : 16  : save_offset_to (off_16)
         ; scene_height   : 16  : save_offset_to (off_17)
         ; hor_scene_off  : 16  : save_offset_to (off_18)
         ; ver_scene_off  : 16  : save_offset_to (off_19)
         |} ->
         []*)

let parse bs off =
  match%bitstring bs with
  | {| text_format : 8
     ; length      : 16 : save_offset_to (off_1)
     ; _           : length * 8 : save_offset_to (off_2), bitstring
     |}
    ->
      (*   let specific = decode_specific_text_config specific (off + off_2) in*)
      [ Node.make ~offset:off 8 "text_format" (String (parse_text_format text_format))
      ; Node.make ~offset:(off + off_1) 16 "text_config_length" (Dec (Uint length))
      ; Node.make
          ~offset:(off + off_2)
          (length * 8)
          "format_specific_text_config"
          (List []) ]
