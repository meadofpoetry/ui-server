open Application_types

let name = "errors"

type params =
  { min : float
  ; max : float
  ; avg : float
  } [@@deriving yojson]

type error =
  { counter   : int
  ; size      : int
  ; params    : params
  ; timestamp : Time.Period.Useconds.t (* TODO to seconds & span -> Time.t *)
  ; peak_flag : bool
  ; cont_flag : bool
  } [@@deriving yojson]

module Video_data = struct
  let name = "video_data"
  type errors =
    { black  : error
    ; luma   : error
    ; freeze : error
    ; diff   : error
    ; blocky : error
    } [@@deriving yojson]
  type t =
    { stream     : Stream.ID.t
    ; channel    : int
    ; pid        : int
    ; errors     : errors
    } [@@deriving yojson]
    
end

module Audio_data = struct
  let name = "audio_data"
  type errors =
    { silence_shortt  : error
    ; silence_moment  : error
    ; loudness_shortt : error
    ; loudness_moment : error
    } [@@deriving yojson]
  type t =
    { stream     : Stream.ID.t
    ; channel    : int
    ; pid        : int
    ; errors     : errors
    } [@@deriving yojson]
end

type labels =
  [ `Black
  | `Luma
  | `Freeze
  | `Diff
  | `Blocky
  | `Silence_shortt
  | `Silence_moment
  | `Loudness_shortt
  | `Loudness_moment
  ] [@@deriving yojson, eq]

let video_data_to_list Video_data.{ stream; channel; pid; errors = { black; luma; freeze; diff; blocky } } =
  [ stream, channel, pid, 0, black
  ; stream, channel, pid, 1, luma
  ; stream, channel, pid, 2, freeze
  ; stream, channel, pid, 3, diff
  ; stream, channel, pid, 4, blocky
  ]

let audio_data_to_list Audio_data.{ stream; channel; pid; errors = { silence_shortt; silence_moment; loudness_shortt; loudness_moment } } =
  [ stream, channel, pid, 5, silence_shortt
  ; stream, channel, pid, 6, loudness_shortt
  ; stream, channel, pid, 7, silence_moment
  ; stream, channel, pid, 8, loudness_moment
  ]

let labels_of_int = function
  | 0 -> `Black
  | 1 -> `Luma
  | 2 -> `Freeze
  | 3 -> `Diff
  | 4 -> `Blocky
  | 5 -> `Silence_shortt
  | 6 -> `Loudness_shortt
  | 7 -> `Silence_moment
  | 8 -> `Loudness_moment
  | _ -> failwith "Qoe_errors.labels_of_int: wrong int"

let labels_to_int = function
  | `Black -> 0
  | `Luma -> 1
  | `Freeze -> 2
  | `Diff -> 3
  | `Blocky -> 4
  | `Silence_shortt -> 5
  | `Loudness_shortt -> 6
  | `Silence_moment -> 7
  | `Loudness_moment -> 8
