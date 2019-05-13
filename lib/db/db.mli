type state

type t = state
                       
module List : sig
  type _ t = [] : unit t | (::) : 'a * 'b t -> ('a * 'b) t

  val (&) : 'a Caqti_type.t -> 'b Caqti_type.t -> ('a * 'b) Caqti_type.t
end

module Request : sig

  type ('a, 'typ) t 

  type ('a,'b,+'c) request = ('a,'b,'c) Caqti_request.t constraint 'c = [<`Many | `One | `Zero]

  module Build : module type of Caqti_request
                                with type ('a,'b,+'c) t := ('a,'b,'c) request

  val (>>=) : ('a, 'typ) t -> ('a -> ('b, 'typ) t) -> ('b, 'typ) t

  val return : 'a -> ('a, [> `Simple]) t

  val exec : ('a, unit, [`Zero]) request -> 'a -> (unit, [> `Simple]) t

  val find : ('a, 'b, [`One | `Zero]) request -> 'a -> ('b option, [> `Simple]) t

  val list : ('a, 'b, [`Many | `One | `Zero]) request
             -> 'a
             -> ('b list, [> `Simple]) t

  val with_trans : ('a, [`Simple]) t -> ('a, [> `Trans]) t
(*
  val run : (module Caqti_lwt.CONNECTION) -> ('a, 'typ) t -> 'a Lwt.t
 *)
end

module Key : sig
  
  type t
     
  val key : ?default:string -> ?primary:bool -> string -> t
    
  val is_primary : t -> bool
    
  val typ : t -> string
    
  val to_string : t -> string
    
end

type keys

val make_keys : ?time_key:string -> (string * Key.t) list -> keys

module type MODEL = sig
  type init
  type names
  val name     : string
  val tables   : init -> names * ((string * keys * (unit, _) Request.t option) list)
end

type error = [ `Db_error of string ]
                  
type conn_error = [ `Db_connection_error of string ]

val pp_error : error Fmt.t

val pp_conn_error : conn_error Fmt.t
                  
module type CONN = sig
  type t
  type init
  type names
  val create   : state -> init -> (t, [> conn_error ]) result Lwt.t
  val request  : t -> ('req,_) Request.t -> 'req Lwt.t
  val delete   : t -> unit Lwt.t
  val names    : t -> names
end

module Make (M : MODEL) : (CONN with type init := M.init and type names := M.names)

val create : role:string
             -> password:string
             -> socket_path:string
             -> maintain:Time.Period.t
             -> cleanup:Time.Period.t
             -> (state, [> error ]) result

val finalize : state -> unit
