open Containers

module Axes = Axes.Cartesian

include Line_dataset

type node =
  [ `Id of string
  | `Canvas of Dom_html.canvasElement Js.t
  | `Context of Dom_html.canvasRenderingContext2D Js.t
  ]

let make (node : node) (config : Line_dataset.Config.t) : Chart.t =
  let node = match node with
    | `Id s -> Ojs.string_to_js s
    | `Canvas c -> Obj.magic c
    | `Context c -> Obj.magic c in
  Line_dataset.Chart.new_chart node config
