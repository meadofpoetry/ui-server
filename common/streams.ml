type video_pid =
  { codec        : string
  ; resolution   : (int * int)
  ; aspect_ratio : (int * int)
  ; interlaced   : string
  ; frame_rate   : float
  } [@@deriving yojson, lens]

type audio_pid =
  { codec       : string
  ; bitrate     : string
  ; channels    : int
  ; sample_rate : int
  } [@@deriving yojson, lens]

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
  } [@@deriving yojson, lens]

type channel =
  { number        : int
  ; service_name  : string
  ; provider_name : string
  ; pids          : pid list
  } [@@deriving yojson, lens]

type stream =
  { id       : int32
  ; input    : string
  ; uri      : string
  ; channels : channel list
  } [@@deriving yojson, lens]

type t = stream list [@@deriving yojson]

open Opt_update

let pid_update _ b = b
  
let channel_update _ b = b

let stream_update _ b = b

let update _ b = b

let default : stream list = []
