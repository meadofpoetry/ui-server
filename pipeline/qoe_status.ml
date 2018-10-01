let name = "stream_lost"

type t =
  { stream  : int
  ; channel : int
  ; pid     : int
  ; playing : bool
  } [@@deriving yojson]

type status_list = t list [@@deriving yojson]
