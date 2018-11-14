open Types

module Options = Pie.Options

let make (node : node) (config : Config.t) : Chart.t =
  let node = match node with
    | `Id s -> Ojs.string_to_js s
    | `Canvas c -> Obj.magic c
    | `Context c -> Obj.magic c in
  Config.set_type config "doughnut";
  Chart.new_chart node config
