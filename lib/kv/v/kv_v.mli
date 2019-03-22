type error = [ `No_value of Kv.RW.key list ]

val pp_error : error Fmt.t

module type RO_V = sig
  type value
  val create : ?default:value -> Kv.RO.t -> Kv.RO.key list -> value option Lwt.t
end

module RO (S : sig
             type t
             val of_string : string -> t
           end) : RO_V with type value := S.t

type 'value rw = < get : 'value Lwt.t; set : 'value -> unit Lwt.t; s : 'value React.S.t >
     
module type RW_V = sig
  type value
  type t = value rw
  val create : ?default:value
               -> Kv.RW.t
               -> Kv.RW.key list
               -> (t, [> error]) result Lwt.t
end

module RW (S : sig
             type t
             val equal : t -> t -> bool
             val of_string : string -> t
             val to_string : t -> string
           end) : RW_V with type value := S.t
