type dataset = private Ojs.t

type t =
  { data : Ojs.t
  ; options : Options.t
  ; type_ : string
  }
val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

val type_ : t -> string
val set_type : t -> string -> unit

val make : ?options:Options.t ->
           type_:string ->
           data:Ojs.t ->
           unit ->
           t [@@js.builder]
