type 'a wrapper =
  { data     : 'a
  ; has_more : bool
  ; total    : (int option [@default None])
  } [@@deriving yojson,make]
