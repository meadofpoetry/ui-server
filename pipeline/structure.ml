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

type structure =
  { id       : int32 [@key "stream"] (* TODO replace by id *)
  ; uri      : string
  ; channels : channel list
  } [@@deriving yojson]

type structure_list = structure list [@@deriving yojson]

type source = Unknown
            | Stream  of Common.Stream.t
            [@@deriving yojson]

type t = { source    : source
         ; structure : structure
         } [@@deriving yojson]

type t_list = t list [@@deriving yojson]

let default : t list = []
