include module type of Qoe_backend_types.Wm.Make (Application_types.Stream.ID)

val default : t

module Annotated : sig

  type raw = t

  type state = [`Active | `Stored ]

  type container =
    { position : position
    ; widgets  : (string * state * widget) list
    }

  type t =
    { resolution : int * int
    ; widgets    : (string * widget) list
    ; layout     : (string * state * container) list
    }

  val equal : t -> t -> bool

  val to_yojson : t -> Yojson.Safe.json

  val of_yojson : Yojson.Safe.json -> (t, string) result

  val annotate : active:raw -> stored:raw -> t

  val update_stored : active:raw -> stored:raw -> [`Changed of raw | `Kept of raw ]
(*
  val filter : select:state -> t -> raw
 *)
end
