type source_state =
  [ `Forbidden
  | `Limited   of int
  | `Unlimited
  ] [@@deriving yojson, eq]

type set_error =
  [ `Not_in_range
  | `Limit_exceeded of (int * int)
  | `Forbidden
  | `Internal_error of string
  ] [@@deriving yojson, eq]

type marker =
  [ `Input of Common.Topology.input * int
  | `Board of int
  ] [@@deriving yojson, eq]

type stream_setting =
  (marker * Common.Stream.t list) list [@@deriving yojson, eq]

type stream_table_row =
  (marker
   * source_state
   * (Common.Url.t option * Common.Stream.t) list) [@@deriving yojson, eq]

type stream_table =
   stream_table_row list [@@deriving yojson ,eq]

let set_error_to_string : set_error -> string  = function
  | `Not_in_range -> "Not in range"
  | `Limit_exceeded (exp,got) ->
     Printf.sprintf "Limit exceeded: got %d streams, \
                     but only %d is available" got exp
  | `Forbidden -> "Forbidden"
  | `Internal_error e -> Printf.sprintf "Internal error: %s" e
