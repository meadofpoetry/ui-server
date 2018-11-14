open Config

type t
val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

val new_chart : Ojs.t -> Config.t -> t [@@js.new "Chart"]

val update : t -> unit [@@js.call]
