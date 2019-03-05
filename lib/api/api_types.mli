type 'a raw =
  { data     : 'a
  ; has_more : bool
  ; order    : [ `Asc | `Desc ]
  }

type ('a,'b) rows =
  | Compressed of 'b
  | Raw        of 'a raw

val raw_to_yojson : ('a -> Yojson.Safe.json) -> 'a raw -> Yojson.Safe.json

val raw_of_yojson : (Yojson.Safe.json -> ('a, string) result)
                    -> Yojson.Safe.json
                    -> ('a raw, string) result

val rows_to_yojson : ('a -> Yojson.Safe.json)
                     -> ('b -> Yojson.Safe.json)
                     -> ('a, 'b) rows
                     -> Yojson.Safe.json

val rows_of_yojson : (Yojson.Safe.json -> ('a, string) result)
                     -> (Yojson.Safe.json -> ('b, string) result)
                     -> Yojson.Safe.json
                     -> (('a,'b) rows, string) result
