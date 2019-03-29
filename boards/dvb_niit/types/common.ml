type id =
  { stream : Application_types.Stream.ID.t
  ; tuner : int
  } [@@deriving yojson, eq]

type 'a ts =
  { data : 'a
  ; timestamp : Time.t
  } [@@deriving yojson, show, eq]
