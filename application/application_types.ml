open Common

type marker =
  [ `Input of Topology.input * int
  | `Board of int
  ] [@@deriving yojson, eq]

type stream_setting =
  (marker * Stream.t list) list [@@deriving yojson, eq]

type stream_table_row =
  (marker
   * Stream.Table.source_state
   * Stream.Table.stream list) [@@deriving yojson, eq]

type stream_table =
   stream_table_row list [@@deriving yojson ,eq]

let set_error_to_string : Stream.Table.set_error -> string =
  function
  | `Not_in_range -> "Not in range"
  | `Limit_exceeded (exp,got) ->
     Printf.sprintf "Limit exceeded: got %d streams, \
                     but only %d is available" got exp
  | `Forbidden -> "Forbidden"
  | `Internal_error e -> Printf.sprintf "Internal error: %s" e
