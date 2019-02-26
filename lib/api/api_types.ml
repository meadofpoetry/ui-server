type 'a compressed =
  { data : 'a
  } [@@deriving yojson]

type 'a raw =
  { data     : 'a
  ; has_more : bool
  ; order    : [ `Asc | `Desc ]
  } [@@deriving yojson]

type ('a,'b) rows =
  | Compressed of 'b compressed
  | Raw        of 'a raw
  [@@deriving yojson]
