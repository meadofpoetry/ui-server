open Qoe_errors_types

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
  ; timestamp : Common.Time.Useconds.t (* TODO to seconds *)
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
    { stream     : int
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
    { stream     : int
    ; channel    : int
    ; pid        : int
    ; errors     : errors
    } [@@deriving yojson]
end

let video_data_to_list Video_data.{ stream; channel; pid; errors = { black; luma; freeze; diff; blocky } } =
  [ stream, channel, pid, 0, black
  ; stream, channel, pid, 1, luma
  ; stream, channel, pid, 2, freeze
  ; stream, channel, pid, 3, diff
  ; stream, channel, pid, 4, blocky
  ]

let audio_data_to_list Audio_data.{ stream; channel; pid; errors = { silence_shortt; silence_moment; loudness_shortt; loudness_moment } } =
  [ stream, channel, pid, 5, silence_shortt
  ; stream, channel, pid, 6, silence_moment
  ; stream, channel, pid, 7, loudness_shortt
  ; stream, channel, pid, 8, loudness_moment
  ]
