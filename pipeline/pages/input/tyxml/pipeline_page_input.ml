let stylesheets = []

let pre_scripts = []

let post_scripts =
  [`Src "/js/pipeline-page-input.js"]

let tabs =
  [ "qoe", "QoE", [Tyxml.Html.toelt @@ Tyxml.Html.txt "this is QoE tab"]
  ]
