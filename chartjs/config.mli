type dataset = private Ojs.t

module Data : sig
  type t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

  val datasets : t -> dataset list
  val set_datasets : t -> dataset list -> unit

  val make : ?datasets:dataset list ->
             unit ->
             t [@@js.builder]

end

type t =
  { data : Data.t
  ; options : Options.t
  }
val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

val data : t -> Data.t
val set_data : t -> Data.t -> unit

val options : t -> Options.t
val set_options : t -> Options.t -> unit

val type_ : t -> string
val set_type : t -> string -> unit

val make : ?options:Options.t ->
           type_:string ->
           data:Data.t ->
           unit ->
           t [@@js.builder]
