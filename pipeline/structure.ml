type video_pid =
  { codec        : string
  ; resolution   : (int * int)
  ; aspect_ratio : (int * int)
  ; interlaced   : string
  ; frame_rate   : float
  } [@@deriving yojson]

type audio_pid =
  { codec       : string
  ; bitrate     : string
  ; channels    : int
  ; sample_rate : int
  } [@@deriving yojson]

type pid_content = Video of video_pid
                 | Audio of audio_pid
                 | Empty
let pid_content_to_yojson = function
  | Empty   -> `String "Empty"
  | Video v -> `Assoc [("Video", (video_pid_to_yojson v))]
  | Audio a -> `Assoc [("Audio", (audio_pid_to_yojson a))]
let pid_content_of_yojson = function
  | `String "Empty" -> Ok(Empty)
  | `Assoc [("Video", v)] ->
     (match video_pid_of_yojson v with
      | Ok v -> Ok(Video v)
      | _    -> Error("failure in video_pid deserialize"))
  | `Assoc [("Audio", a)] ->
     (match audio_pid_of_yojson a with
      | Ok a -> Ok(Audio a)
      | _    -> Error("failure in audio_pid deserialize"))
  | _ -> Error("failure in pid_content deserialize")

type pid =
  { pid              : int
  ; to_be_analyzed   : bool
  ; content          : pid_content
  ; stream_type      : int
  ; stream_type_name : string
  } [@@deriving yojson]

type channel =
  { number        : int
  ; service_name  : string
  ; provider_name : string
  ; pids          : pid list
  } [@@deriving yojson]

type structure =
  { id       : int32 
  ; uri      : string
  ; channels : channel list
  } [@@deriving yojson]

type source = Unknown
            | Stream  of Common.Stream.t
            [@@deriving yojson]

type packed = { source    : source
              ; structure : structure
              } [@@deriving yojson]
type t = packed

module Structures = struct
  type t   = structure list [@@deriving yojson]
  let name = "structures"
end
       
module Streams = struct
  type t   = packed list [@@deriving yojson]
  let name = "streams"
  let default : t = []

  let unwrap : t -> structure list =
    List.map (fun { source; structure } -> structure )
end
