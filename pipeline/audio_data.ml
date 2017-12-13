type params =
  { shortt     : float
  ; moment     : float
  ; time       : int64
  } [@@deriving yojson]

type error_flags =
  { cont : bool
  ; peak : bool
  ; time : int64
  } [@@deriving yojson]

type errors =
  { silence  : error_flags list
  ; loudness : error_flags list
  } [@@deriving yojson]

type t =
  { stream     : int
  ; channel    : int
  ; pid        : int
  ; parameters : params list
  ; errors     : errors
  } [@@deriving yojson]
