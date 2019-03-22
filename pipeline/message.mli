type chan

type 'a t = ('a, string) Lwt_result.t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val return : 'a -> 'a t

(* 
val call : name:string -> meth:string
           -> ('a -> Yojson.Safe.json)
           -> (Yojson.Safe.json -> 'b res)
           -> chan
           -> ('a -> 'b t)
 *)
  
val create_channel : Lwt_mutex.t
                     -> (Yojson.Safe.json -> (Yojson.Safe.json, exn) result Lwt.t)
                     -> chan

module Protocol : sig
  val ready : chan -> unit -> unit t
  val stream_parser_get : chan -> unit -> Structure.t list t
  val graph_get_structure : chan -> unit -> Structure.t list t
  val graph_apply_structure :chan
                             -> ?options:Structure.t list Kv_v.rw
                             -> Structure.t list
                             -> unit t
  val wm_get_layout : chan -> unit -> Wm.t t
  val wm_apply_layout : chan
                        -> ?options:Wm.t Kv_v.rw
                        -> Wm.t
                        -> unit t
end
