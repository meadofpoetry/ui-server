type t =
  { offset : int
  ; length : int
  ; name : string
  ; value : value * string option }

and integer =
  | Bool of bool
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Uint of int
  | Uint32 of int32
  | Uint64 of int64

and value =
  | List of t list
  | Bytes of int list
  | String of string
  | Bits of integer
  | Dec of integer
  | Hex of integer
  | Time of Time.t
  | Duration of Time.Period.t
[@@deriving yojson]

let make ?parsed ~offset length name value : t =
  {offset; length; name; value = value, parsed}
