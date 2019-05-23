let name = "MPEG-2_AAC_audio_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| profile        : 8
     ; channel_config : 8 : save_offset_to (off_1)
     ; add_info       : 8 : save_offset_to (off_2)
     |} ->
    [ Node.make ~offset:off 8 "MPEG-2_AAC_profile" (Hex (Int profile))
    ; Node.make ~offset:(off + off_1) 8 "MPEG-2_AAC_channel_configuration" (Hex (Int channel_config))
    ; Node.make ~offset:(off + off_2) 8 "MPEG-2_AAC_additional_information" (Hex (Int add_info))
    ]
