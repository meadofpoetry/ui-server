let name = "video_data"

type params =
  { min : float
  ; max : float
  ; avg : float
  } [@@deriving yojson]

type error =
  { counter   : int
  ; size      : int
  ; params    : params
  ; timestamp : int64
  ; peak_flag : bool
  ; cont_flag : bool
  } [@@deriving yojson]

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
