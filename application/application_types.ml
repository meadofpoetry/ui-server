type source_state = [ `Forbidden
                    | `Limited   of int
                    | `Unlimited
                    ] [@@deriving yojson]

type set_error = [ `Not_in_range
                 | `Limit_exceeded of (int * int)
                 | `Forbidden
                 | `Internal_error of string
                 ] [@@deriving yojson]

type marker = [ `Input of Common.Topology.input * int
              | `Board of int
              ] [@@deriving yojson]

type stream_setting = (marker * Common.Stream.t list) list [@@deriving yojson]
                    
type stream_table   = (marker * source_state * (Common.Url.t option * Common.Stream.t) list) list [@@deriving yojson]
