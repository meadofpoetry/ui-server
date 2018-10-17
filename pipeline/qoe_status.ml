let name = "stream_lost"

type t =
  { stream  : Common.Stream.ID.t
  ; channel : int
  ; pid     : int
  ; playing : bool
  } [@@deriving yojson]

type status_list = t list [@@deriving yojson]
