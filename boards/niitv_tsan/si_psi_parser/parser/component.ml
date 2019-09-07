let name = "component_descriptor"

(* page 42 EN 300 468 *)
let parse ~stream_content ~stream_content_ext ~component_type =
  match stream_content with
  | 0x0 -> (
    match stream_content_ext with
    | x when x >= 0x0 && x <= 0xF -> (
      match component_type with
      | x when x >= 0x00 && x <= 0xFF -> "reserved for future use"
      | _ -> assert false)
    | _ -> assert false)
  | 0x1 -> (
    match component_type with
    | 0x00 -> "reserved for future use"
    | 0x01 -> "MPEG-2 video, 4:3 aspect ratio, 25 Hz"
    | 0x02 -> "MPEG-2 video, 16:9 aspect ratio with pan vectors, 25 Hz"
    | 0x03 -> "MPEG-2 video, 16:9 aspect ratio without pan vectors, 25 Hz"
    | 0x04 -> "MPEG-2 video, > 16:9 aspect ratio, 25 Hz"
    | 0x05 -> "MPEG-2 video, 4:3 aspect ratio, 30 Hz"
    | 0x06 -> "MPEG-2 video, 16:9 aspect ratio with pan"
    | 0x07 -> "MPEG-2 video, 16:9 aspect ratio without pan"
    | 0x08 -> "MPEG-2 video, > 16:9 aspect ratio, 30 Hz"
    | 0x09 -> "MPEG-2 high definition video, 4:3 aspect ratio"
    | 0x0A -> "MPEG-2 high definition video, 16:9 aspect ratio with pan vectors, 25 Hz "
    | 0x0B ->
        "MPEG-2 high definition video, 16:9 aspect ratio without pan vectors, 25 Hz"
    | 0x0C -> "MPEG-2 high definition video, > 16:9 aspect ratio, 25 Hz"
    | 0x0D -> "MPEG-2 high definition video, 4:3 aspect ratio, 30 Hz"
    | 0x0E -> "MPEG-2 high definition video, 16:9 aspect ratio with pan vectors, 30 Hz"
    | 0x0F ->
        "MPEG-2 high definition video, 16:9 aspect ratio without pan vectors, 30 Hz"
    | 0x10 -> "MPEG-2 high definition video, > 16:9 aspect ratio, 30 Hz"
    | 0xFF -> "reserved for future use"
    | x when x >= 0x11 && x <= 0xAF -> "reserved for future use"
    | x when x >= 0xB0 && x <= 0xFE -> "user defined"
    | _ -> assert false)
  | 0x2 -> (
    match component_type with
    | 0x00 -> "reserved for future use"
    | 0x01 -> "MPEG-1 Layer 2 audio, single mono channel"
    | 0x02 -> "MPEG-1 Layer 2 audio, dual mono channel"
    | 0x03 -> "MPEG-1 Layer 2 audio, stereo (2 channel)"
    | 0x04 -> "MPEG-1 Layer 2 audio, multi-lingual, multi-channel"
    | 0x05 -> "MPEG-1 Layer 2 audio, surround sound"
    | 0x40 -> "MPEG-1 Layer 2 audio description for the visually impaired"
    | 0x41 -> "MPEG-1 Layer 2 audio for the hard of hearing"
    | 0x42 -> "receiver-mix supplementary audio as per annex E of ETSI TS 101 154"
    | 0x47 -> "MPEG-1 Layer 2 audio, receiver-mix audio description"
    | 0x48 -> "MPEG-1 Layer 2 audio, broadcast-mix audio description"
    | 0xFF -> "reserved for future use"
    | 0x43 | 0x44 | 0x45 | 0x46 -> "reserved for future use"
    | x when x >= 0x06 && x <= 0x3F -> "reserved for future use"
    | x when x >= 0x49 && x <= 0xAF -> "reserved for future use"
    | x when x >= 0xB0 && x <= 0xFE -> "user-defined"
    | _ -> assert false)
  | 0x3 -> (
    match component_type with
    | 0x01 -> "EBU Teletext subtitles"
    | 0x02 -> "associated EBU Teletext"
    | 0x03 -> "VBI data"
    | 0x10 -> "DVB subtitles (normal) with no monitor aspect ratio criticality"
    | 0x11 -> "DVB subtitles (normal) for display on 4:3 aspect ratio monitor"
    | 0x12 -> "DVB subtitles (normal) for display on 16:9 aspect ratio monitor"
    | 0x13 -> "DVB subtitles (normal) for display on 2.21:1 aspect ratio monitor"
    | 0x14 -> "DVB subtitles (normal) for display on a high definition monitor"
    | 0x15 ->
        "DVB subtitles (normal) with plano-stereoscopic disparity for display on a high \
         definition monitor"
    | 0x20 ->
        "DVB subtitles (for the hard of hearing) with no monitor aspect ratio criticality"
    | 0x21 ->
        "DVB subtitles (for the hard of hearing) for  display on 4:3 aspect ratio monitor"
    | 0x22 ->
        "DVB subtitles (for the hard of hearing) for display on 16:9 aspect ratio monitor"
    | 0x23 ->
        "DVB subtitles (for the hard of hearing) for display on 2.21:1 aspect ratio \
         monitor"
    | 0x24 ->
        "DVB subtitles (for the hard of hearing) for display on a high definition monitor"
    | 0x25 ->
        "DVB subtitles (for the hard of hearing) with plano-stereoscopic disparity for \
         display on a high definition monitor"
    | 0x30 -> "open (in-vision) sign language interpretation for the deaf"
    | 0x31 -> "closed sign language interpretation for the deaf"
    | 0x40 -> "video up-sampled from standard definition source material"
    | 0x80 -> "dependent SAOC-DE data stream"
    | 0xFF -> "reserved for future use"
    | x
      when (x >= 0x04 && x <= 0x0F)
           || (x >= 0x16 && x <= 0x1F)
           || (x >= 0x26 && x <= 0x2F)
           || (x >= 0x32 && x <= 0x3F)
           || (x >= 0x41 && x <= 0x7F)
           || (x >= 0x81 && x <= 0xAF)
           || x = 0xFF
           || x = 0x00 ->
        "reserved for future use"
    | x when x >= 0xB0 && x <= 0xFE -> "user defined"
    | _ -> assert false)
  | 0x4 -> (
    match component_type with
    | x when x >= 0x00 && x <= 0x7F -> "reserved for AC-3 audio modules"
    | x when x >= 0x80 && x <= 0xFF -> "reserved for enhanced AC-3 audio modules"
    | _ -> assert false)
  | 0x5 -> (
    match component_type with
    | 0x01 -> "H.264/AVC standard definition video, 4:3 aspect ratio, 25 Hz"
    | 0x03 -> "H.264/AVC standard definition video, 16:9 aspect ratio, 25 Hz"
    | 0x04 -> "H.264/AVC standard definition video, > 16:9 aspect ratio, 25 Hz"
    | 0x05 -> "H.264/AVC standard definition video, 4:3 aspect ratio, 30 Hz"
    | 0x07 -> "H.264/AVC standard definition video, 16:9 aspect ratio, 30 Hz"
    | 0x08 -> "H.264/AVC standard definition video, > 16:9 aspect ratio, 30 Hz"
    | 0x0B -> "H.264/AVC high definition video, 16:9 aspect ratio, 25 Hz"
    | 0x0C -> "H.264/AVC high definition video, > 16:9 aspect ratio, 25 Hz"
    | 0x0F -> "H.264/AVC high definition video, 16:9 aspect ratio, 30 Hz"
    | 0x10 -> "H.264/AVC high definition video, > 16:9 aspect ratio, 30 Hz"
    | 0x80 ->
        "H.264/AVC plano-stereoscopic frame compatible high definition video, 16:9 \
         aspect ratio, 25 Hz, Side-by-Side"
    | 0x81 ->
        "H.264/AVC plano-stereoscopic frame compatible high definition video, 16:9 \
         aspect ratio, 25 Hz, Top-and-Bottom"
    | 0x82 ->
        "H.264/AVC plano-stereoscopic frame compatible high definition video, 16:9 \
         aspect ratio, 30 Hz, Side-by-Side"
    | 0x83 ->
        "H.264/AVC stereoscopic frame compatible high definition video, 16:9 aspect \
         ratio, 30 Hz, Top-and-Bottom"
    | 0x84 -> "H.264/MVC dependent view, planostereoscopic service compatible video"
    | x
      when x = 0x00
           || x = 0x02
           || x = 0x06
           || x = 0xFF
           || (x >= 0x09 && x <= 0x0A)
           || (x >= 0x0D && x <= 0x0E)
           || (x >= 0x11 && x <= 0x7F)
           || (x >= 0x85 && x <= 0xAF) ->
        "reserved for future use"
    | x when x >= 0xB0 && x <= 0xFE -> "user defined"
    | _ -> assert false)
  | 0x6 -> (
    match component_type with
    | 0x01 -> "HE AAC audio, single mono channel"
    | 0x03 -> "HE AAC audio, stereo"
    | 0x05 -> "HE AAC audio, surround sound"
    | 0x40 -> "HE AAC audio description for the visually impaired"
    | 0x41 -> "HE AAC audio for the hard of hearing"
    | 0x42 ->
        "HE AAC receiver-mix supplementary audio as per annex E of ETSI TS 101 154 [9]"
    | 0x43 -> "HE AAC v2 audio, stereo"
    | 0x44 -> "HE AAC v2 audio description for the visually impaired"
    | 0x45 -> "HE AAC v2 audio for the hard of hearing"
    | 0x46 ->
        "HE AAC v2 receiver-mix supplementary audio as per annex E of ETSI TS 101 154 [9]"
    | 0x47 -> "HE AAC receiver-mix audio description for the visually impaired"
    | 0x48 -> "HE AAC broadcast-mix audio description for the visually impaired"
    | 0x49 -> "HE AAC v2 receiver-mix audio description for the visually impaired"
    | 0x4A -> "HE AAC v2 broadcast-mix audio description for the visually impaired"
    | 0xA0 -> "HE AAC, or HE AAC v2 with SAOC-DE ancillary data"
    | x
      when x = 0xFF
           || x = 0x00
           || x = 0x02
           || x = 0x04
           || (x >= 0x06 && x <= 0x3F)
           || (x >= 0x4B && x <= 0x9F)
           || (x >= 0xA1 && x <= 0xAF) ->
        "reserved for future use"
    | x when x >= 0xB0 && x <= 0xFE -> "user defined"
    | _ -> assert false)
  | 0x7 -> (
    match component_type with
    | x when x >= 0x00 && x <= 0x7F -> "reserved for DTS® and DTS-HD® audio modes"
    | x when x >= 0x80 && x <= 0xFF -> "reserved for future use"
    | _ -> assert false)
  | 0x8 -> (
    match component_type with
    | 0x00 -> "reserved for future use"
    | 0x01 -> "DVB SRM data"
    | x when x >= 0x02 && x <= 0xFF -> "reserved for DVB CPCM modes"
    | _ -> assert false)
  | 0x9 -> (
    match stream_content_ext with
    | 0x0 -> (
      match component_type with
      | 0x00 -> "HEVC Main Profile high definition video, 50 Hz"
      | 0x01 -> "HEVC Main 10 Profile high definition video, 50 Hz"
      | 0x02 -> "HEVC Main Profile high definition video, 60 Hz"
      | 0x03 -> "HEVC Main 10 Profile high definition video, 60 Hz"
      | 0x04 -> "HEVC ultra high definition video"
      | x when x >= 0x05 && x <= 0xFF -> "reserved for future use"
      | _ -> assert false)
    | 0x1 -> (
      match component_type with
      | 0x00 -> "AC-4 main audio, mono"
      | 0x01 -> "AC-4 main audio, mono, dialogue enhancement enabled"
      | 0x02 -> "AC-4 main audio, stereo"
      | 0x03 -> "AC-4 main audio, stereo, dialogue enhancement enabled"
      | 0x04 -> "AC-4 main audio, multichannel"
      | 0x05 -> "AC-4 main audio, multichannel, dialogue enhancement enabled"
      | 0x06 -> "AC-4 broadcast-mix audio description, mono, for the visually impaired"
      | 0x07 ->
          "AC-4 broadcast-mix audio description, mono, for the visually impaired, \
           dialogue enhancement enabled"
      | 0x08 -> "AC-4 broadcast-mix audio description, stereo, for the visually impaired"
      | 0x09 ->
          "AC-4 broadcast-mix audio description, stereo, for the visually impaired, \
           dialogue enhancement enabled"
      | 0x0A ->
          "AC-4 broadcast-mix audio description, multichannel, for the visually impaired"
      | 0x0B ->
          "AC-4 broadcast-mix audio description, multichannel, for the visually \
           impaired, dialogue enhancement enabled"
      | 0x0C -> "AC-4 receiver-mix audio description, mono, for the visually impaired"
      | 0x0D -> "AC-4 receiver-mix audio description, stereo, for the visually impaired"
      | x when x >= 0x0E && x <= 0xFF -> "reserved for future use"
      | _ -> assert false)
    | x when x >= 0x2 && x <= 0xF -> (
      match component_type with
      | x when x >= 0x00 && x <= 0xFF -> "reserved for future use"
      | _ -> assert false)
    | _ -> assert false)
  | 0xA -> (
    match stream_content_ext with
    | x when x >= 0x0 && x <= 0xF -> (
      match component_type with
      | x when x >= 0x00 && x <= 0xFF -> "reserved for future use"
      | _ -> assert false)
    | _ -> assert false)
  | 0xB -> (
    match stream_content_ext with
    | x when x >= 0x0 && x <= 0xE -> (
      match component_type with
      | x when x >= 0x00 && x <= 0xFF -> "reserved for future use"
      | _ -> assert false)
    | 0xF -> (
      match component_type with
      | 0x00 -> "less than 16:9 aspect ratio"
      | 0x01 -> "16:9 aspect ratio"
      | 0x02 -> "greater than 16:9 aspect ratio"
      | 0x03 -> "plano-stereoscopic top and bottom (TaB) frame-packing"
      | x when x >= 0x04 && x <= 0xFF -> "reserved for future use"
      | _ -> assert false)
    | _ -> assert false)
  | x when x >= 0xC && x <= 0xF -> (
    match component_type with
    | x when x >= 0x00 && x <= 0xFF -> "user defined"
    | _ -> assert false)
  | _ -> assert false

let parse bs off =
  match%bitstring bs with
  | {| stream_content_ext : 4
     ; stream_content     : 4  : save_offset_to (off_1)
     ; component_type     : 8  : save_offset_to (off_2)
     ; component_tag      : 8  : save_offset_to (off_3)
     ; lang_code          : 24 : save_offset_to (off_4), bitstring
     ; rest               : -1 : save_offset_to (off_5), bitstring
     |}
    ->
      let parsed_lang, lang_code = Language_code.parse lang_code in
      let parsed = parse ~stream_content_ext ~stream_content ~component_type in
      let text =
        match Text_decoder.decode @@ Util.Bitstring.to_cstruct rest with
        | Ok s -> s
        | Error _ -> "Unable to decode"
      in
      [ Node.make ~offset:off 4 "stream_content_ext" (Hex (Int stream_content_ext))
      ; Node.make ~offset:(off + off_1) 4 "stream_content" (Hex (Int stream_content))
      ; Node.make
          ~parsed
          ~offset:(off + off_2)
          8
          "component_type"
          (Hex (Int component_type))
      ; Node.make ~offset:(off + off_3) 8 "component_tag" (Hex (Int component_tag))
      ; Node.make
          ~parsed:parsed_lang
          ~offset:(off + off_4)
          24
          "ISO_639_language_code"
          (Hex (Int lang_code))
      ; Node.make
          ~offset:(off + off_5)
          (Bitstring.bitstring_length bs - 48)
          "text"
          (String text) ]
