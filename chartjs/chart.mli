open Config

type t

val new_chart : Ojs.t -> Config.t -> t [@@js.new "Chart"]
