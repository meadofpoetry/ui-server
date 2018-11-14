open Containers

module Axes = Axes
module Options = Options
module Config = Config

(** Chart types *)
module Line = struct

  module Dataset = struct
    include Line.Dataset

    let coerce : t -> Config.dataset = Obj.magic
  end

  include (Line : module type of Line with module Dataset := Dataset)

end

type node =
  [ `Id of string
  | `Canvas of Dom_html.canvasElement Js.t
  | `Context of Dom_html.canvasRenderingContext2D Js.t
  ]

let make (node : node) (config : Config.t) : Chart.t =
  let node = match node with
    | `Id s -> Ojs.string_to_js s
    | `Canvas c -> Obj.magic c
    | `Context c -> Obj.magic c in
  Chart.new_chart node config
