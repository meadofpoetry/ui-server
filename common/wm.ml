(* NOTE: just an assumption *)
type widget_type = Video
                 | Audio
                 | Subtitles
                 | Teletext
                 | Qos          (* qos errors/events *)
                 | Qoe          (* qoe errors/events *)
                 | Clock        (* digital or analog clock *)
                 | Text         (* static text *)
                 | Image        (* image (from file or from url, maybe) *)
                 | Source       (* text with channel(stream,input,etc) name*)
                 | Eit          (* text with EIT info *)
                 | Service_info (* text with service desrciption (resolution,codec,etc) *)
                 | Icons_bar    (* status bar with availability indication of eit, scte35, teletext etc  *)
[@@deriving yojson]

type background = (* NOTE incomplete *)
  { color : int } [@@deriving yojson]

type position =
  { left   : int
  ; top    : int
  ; right  : int
  ; bottom : int
  } [@@deriving yojson]

type widget =
  { type_    : widget_type [@key "type"]
  ; position : position
  } [@@deriving yojson]

type window =
  { position : position
  ; widgets  : (string * widget) list option
  } [@@deriving yojson]

type t =
  { background : background
  ; resolution : int * int
  ; windows    : (string * window) list
  ; widgets    : (string * widget) list
  ; layout     : (string * window) list
  } [@@deriving yojson]

let update _ b = b
