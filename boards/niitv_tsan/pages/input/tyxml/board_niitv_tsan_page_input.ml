module type S = sig
  val topo_board : Application_types.Topology.topo_board
end

module Make(M : S) = struct

  let stylesheets = []

  let pre_scripts = []

  let post_scripts =
    [(* `Src "/js/board-niitv-tsan-page-input.js" *)]

  let tabs =
    [ "tsan", "QoS", [Tyxml.Html.toelt @@ Tyxml.Html.txt "this is tsan tab"]
    ]

end
