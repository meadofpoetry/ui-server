module type S = sig
  val topo_board : Application_types.Topology.topo_board
end

module Make(M : S) = struct

  let stylesheets = []

  let pre_scripts = []

  let post_scripts =
    [`Src "/js/board-dektec-dtm3200-page-input.js"]

  let tabs =
    [ "tsoip", "TSoIP", [Tyxml.Html.toelt @@ Tyxml.Html.txt "this is DTM-3200 tab"]
    ]

end
