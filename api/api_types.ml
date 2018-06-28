type ('a,'b) rows =
  | Compressed of { data: 'b }
  | Raw of { data: 'a; has_more: bool; order: [`Asc | `Desc] }
  [@@deriving yojson]
