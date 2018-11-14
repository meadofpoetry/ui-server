type dataset = private Ojs.t

module Data : sig
  type t

  val datasets : t -> dataset list
  val set_datasets : t -> dataset list -> unit

  val make : ?datasets:dataset list ->
             unit ->
             t [@@js.builder]

end

type t = private Ojs.t
val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

val make : ?data:Data.t ->
           ?options:Options.t ->
           type_:string ->
           unit ->
           t [@@js.builder]
