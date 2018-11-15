open Types

module Dataset = Pie_.Dataset

module Data = Pie_.Data

include Chart

let config (t : Chart.t) : Config.t =
  Ojs.get (Chart.t_to_js t) "config"
  |> Obj.magic
