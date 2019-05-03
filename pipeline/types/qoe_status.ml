open Application_types

let name = "stream_lost"

type t =
  { stream  : Stream.ID.t
  ; channel : int
  ; pid     : int
  ; playing : bool
  } [@@deriving yojson, ord]
