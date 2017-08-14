type video_pid =
  { codec      : string
  ; resolution : (int * int) option [@default None]
  ; aspect     : (int * int) option [@default None]
  ; interlaced : string option [@default None]
  ; frame_rate : float option [@default None]
  } [@@deriving yojson, lens { optional = true } ]

type audio_pid =
  { codec       : string
  ; bitrate     : string option [@default None]
  ; sample_rate : int option [@default None]
  } [@@deriving yojson, lens { optional = true } ]

type pid_content = Video of video_pid
                 | Audio of audio_pid
                              [@@deriving yojson]

type pid =
  { pid              : int
  ; to_be_analyzed   : bool option [@default None]
  ; pid_content      : pid_content option [@default None]
  ; stream_type      : int option [@default None]
  ; stream_type_name : string option [@default None]
  } [@@deriving yojson, lens { optional = true } ]

type channel =
  { number        : int
  ; service_name  : string option [@default None]
  ; provider_name : string option [@default None]
  ; pids          : pid list
  } [@@deriving yojson, lens { optional = true } ]

type stream =
    { input    : string
    ; uri      : string (* ? *)
    ; channels : channel list
    } [@@deriving yojson, lens { optional = true } ]

type t =
  { prog_list         : stream list option [@default None]
  } [@@deriving yojson, lens { optional = true } ]

open Opt_update

let pid_update _ b = b
  
let channel_update a b =
  { number        = b.number
  ; service_name  = a.service_name <+> b.service_name
  ; provider_name = a.provider_name <+> b.provider_name
  ; pids          = b.pids
  }

let stream_update _ b = b

let update _ b =
  { prog_list  = b.prog_list
  }

let default = { prog_list = None }
