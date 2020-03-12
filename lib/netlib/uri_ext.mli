include module type of Uri with type t = Uri.t

module Scheme : sig
  type uri = t

  type t = string

  val ws : t

  val wss : t

  val http : t

  val https : t

  val udp : t

  val ws_pat : t list

  val http_pat : t list

  val is : uri -> t -> bool
end

module Path : sig
  type uri = t

  type t

  (* type templ*)

  module Format : sig
    type (_, _) t

    type _ fmt =
      | String : string fmt
      | Int : int fmt
      | Int32 : int32 fmt
      | Uuid : Uuidm.t fmt
      | Bool : bool fmt
      | Any : unit fmt

    type paths

    val equal : ('a, 'b) t -> ('c, 'd) t -> bool

    val templates : unit -> paths

    val store_template : paths -> ('a, 'b) t -> unit

    val has_template : paths -> ('a, 'b) t -> bool

    val empty : ('a, 'a) t

    val ( @/ ) : string -> ('a, 'b) t -> ('a, 'b) t

    val ( ^/ ) : 'a fmt -> ('b, 'c) t -> ('a -> 'b, 'c) t

    val ( / ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

    (*
    val to_templ : (_,_) t -> templ
 *)
    val doc : ('a, 'b) t -> string

    val of_string : string -> ('a, 'a) t

    (*
    val scan_unsafe : string list -> ('a,'b) t -> 'a -> 'b
 *)
    val kprint : (string list -> 'b) -> ('a, 'b) t -> 'a
  end

  val empty : t

  val equal : t -> t -> bool

  (*
  val to_templ : t -> templ

  val templ_compare : templ -> templ -> int
     *)
  val next : t -> string option * t

  val of_uri : uri -> t

  val of_string : string -> t

  val to_string : t -> string

  val concat : t -> t -> t
end

module Query : sig
  module type Show = sig
    type t

    val typ : string

    val to_string : t -> string

    val of_string : string -> t
  end

  module type Convert = sig
    type t

    val typ : string

    val to_query : t -> string list option

    val of_query : string list option -> t
  end

  module String : Show with type t = string

  module Int : Show with type t = int

  module Int32 : Show with type t = int32

  module Int64 : Show with type t = int64

  module Float : Show with type t = float

  module Bool : Show with type t = bool

  module Either (L : Show) (R : Show) :
    Show with type t = [ `Left of L.t | `Right of R.t ]

  module List (E : Show) : Convert with type t = E.t list

  module Single (E : Show) : Convert with type t = E.t

  module Option (E : Show) : Convert with type t = E.t option

  type t

  type (_, _) format =
    | ( :: ) :
        (string * (module Convert with type t = 'a)) * ('b, 'c) format
        -> ('a -> 'b, 'c) format
    | [] : ('c, 'c) format

  val empty : ('a, 'a) format

  val make : ('a, t) format -> 'a

  val concat : t -> t -> t

  val doc : ('a, 'b) format -> (string * string) list
end

module Dispatcher : sig
  type uri = t

  type 'a node

  type 'a t

  val empty : 'a t

  val make :
    ?docstring:string ->
    path:('a, 'b) Path.Format.t ->
    query:('b, 'c) Query.format ->
    'a ->
    'c node

  val map_node : ('a -> 'b) -> 'a node -> 'b node

  val prepend : Path.t -> 'a node -> 'a node

  exception Ambiguity of string

  (* raises Ambiguity on ambiguous path *)
  val add : 'a t -> 'a node -> 'a t

  (* raises Ambiguity on ambiguous path *)
  val merge : 'a t -> (Path.t * 'a t list) list -> 'a t

  (* raises Ambiguity on ambiguous path *)
  val concat : 'a t list -> 'a t

  val dispatch : default:'a -> 'a t -> uri -> 'a

  val doc : 'a t -> string list
end

(* TODO move to a more approp place *)
val typ : string

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, string) result

val host_v4 : t -> Ipaddr_ext.V4.t option

val with_host_v4 : t -> Ipaddr_ext.V4.t -> t

val with_path_parsed : t -> Path.t -> t

val construct :
  ?scheme:Scheme.t ->
  ?host:string ->
  ?port:int ->
  path:('a, 'b) Path.Format.t ->
  query:('b, t) Query.format ->
  'a

val kconstruct :
  ?scheme:Scheme.t ->
  ?host:string ->
  ?port:int ->
  f:(t -> 'c) ->
  path:('a, 'b) Path.Format.t ->
  query:('b, 'c) Query.format ->
  'a
