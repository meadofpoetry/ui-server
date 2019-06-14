include module type of Qoe_backend_types.Structure.Make
                         (Application_types.Stream.ID)
                         (Netlib.Uri)

val pids : t list -> (Application_types.Stream.ID.t * int * int) list

module Annotated : sig
  
  type state = [`Active_and_stored | `Avail | `Stored ]
             
  type raw = t list

  type channel =
    { number        : int
    ; service_name  : string
    ; provider_name : string
    ; pids          : (state * pid) list
    }

  type structure =
    { id       : Application_types.Stream.t
    ; uri      : Netlib.Uri.t
    ; channels : (state * channel) list
    }

  type t = structure list
           
  val equal : t -> t -> bool

  val to_yojson : t -> Yojson.Safe.json

  val of_yojson : Yojson.Safe.json -> (t, string) result

  val annotate : active:raw -> avail:raw -> stored:raw -> t

  val update_stored : active:raw -> avail:raw -> stored:raw -> [`Changed of raw | `Kept of raw ]

  val filter : select:state -> t -> raw

end
  
module Many : sig

  type nonrec t = t list
                
  val default : t

  val equal : t -> t -> bool

  val to_yojson : t -> Yojson.Safe.json

  val of_yojson : Yojson.Safe.json -> (t, string) result

  val to_string : t -> string

  val of_string : string -> t

end
