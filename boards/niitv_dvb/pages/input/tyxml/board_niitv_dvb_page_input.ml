module type S = sig
  val topo_board : Application_types.Topology.topo_board
end

module Make(M : S) = struct

  let stylesheets = []

  let pre_scripts = []

  let post_scripts =
    [`Src "/js/board-niitv-dvb-page-input.js"]

  let tabs =
    [ "rf", "RF", [Tyxml.Html.toelt @@ Tyxml.Html.txt "this is rf tab"]
    ]

end
