[@@@ocaml.warning "-60"]

module Options = struct

  include Options

  module Axes      = Axes
  module Hover     = Hover
  module Title     = Title
  module Layout    = Layout
  module Animation = Animation
  module Elements  = Elements
  module Legend    = Legend
  module Tooltip   = Tooltip

end

module Line = Line

let create_canvas ?id ?width ?height () =
  let open Tyxml_js.Html in
  canvas ~a:(CCOpt.map_or ~default:[] (fun x -> [a_id x]) id
             |> (fun attrs -> CCOpt.map_or ~default:attrs (fun x -> (a_width x) :: attrs) width)
             |> (fun attrs -> CCOpt.map_or ~default:attrs (fun x -> (a_height x) :: attrs) height))
         []
