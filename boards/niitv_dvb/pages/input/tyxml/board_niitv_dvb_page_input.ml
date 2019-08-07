module type S = sig
  val topo_board : Application_types.Topology.topo_board
end

module Make(M : S) = struct

  let stylesheets = []

  let pre_scripts =
    [ `Src "/js/moment.min.js"
    ; `Src "/js/Chart.min.js"
    ; `Src "/js/chartjs-plugin-streaming.min.js"
    ; `Src "/js/chartjs-plugin-datalabels.min.js"
    ]

  let post_scripts =
    [`Src "/js/board-niitv-dvb-page-input.js"]

  let tabs = [ "rf", "RF", []]

end
