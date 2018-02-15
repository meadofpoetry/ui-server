let name = "video_data"

type params =
  { frozen_pix : float
  ; black_pix  : float
  ; blocks     : float
  ; avg_bright : float
  ; avg_diff   : float
  ; time       : int64
  } [@@deriving yojson]

type error_flags =
  { cont : bool
  ; peak : bool
  ; time : int64
  } [@@deriving yojson]

type errors =
  { black  : error_flags list
  ; luma   : error_flags list
  ; freeze : error_flags list
  ; diff   : error_flags list
  ; blocky : error_flags list
  } [@@deriving yojson]

type t =
  { stream     : int
  ; channel    : int
  ; pid        : int
  ; parameters : params list
  ; errors     : errors
  } [@@deriving yojson]
