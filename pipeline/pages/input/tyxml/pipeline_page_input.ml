let stylesheets = []

let pre_scripts =
  [ `Src "/js/moment.min.js"
  ; `Src "/js/Chart.min.js"
  ; `Src "/js/chartjs-plugin-streaming.min.js"
  ]

let post_scripts =
  [`Src "/js/pipeline-page-input.js"]

let tabs =
  [ "qoe", "QoE", []
  ]
