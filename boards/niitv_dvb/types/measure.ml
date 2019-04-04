type t =
  { lock : bool
  ; power : float option
  ; mer : float option
  ; ber : float option
  ; freq : int option
  ; bitrate : int option
  } [@@deriving yojson, show, eq]
