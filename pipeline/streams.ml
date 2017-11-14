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
                 [@@deriving yojson]

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

type stream =
  { stream   : int32 (* TODO replace by id *)
  ; uri      : string
  ; channels : channel list
  } [@@deriving yojson]

type streams = stream list [@@deriving yojson]

type source = Unknown
            | Stream  of Common.Stream.t
            [@@deriving yojson]

type entry = { source : source
             ; stream : stream
             } [@@deriving yojson]

type entries = entry list [@@deriving yojson]

open Opt_update

let pid_update _ b = b
  
let channel_update _ b = b

let stream_update _ b = b

let update _ b = b

let default : entries = []
