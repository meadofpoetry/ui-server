let name = "MPEG-4_video_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| mpeg_4_visual : 8 |} ->
    [Node.make ~offset:off 8 "MPEG-4_visual_profile_and_level" (Hex (Int mpeg_4_visual))]
