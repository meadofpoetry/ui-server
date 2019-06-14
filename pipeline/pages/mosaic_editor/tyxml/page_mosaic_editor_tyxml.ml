open Components_tyxml

module CSS = struct
  let root = "mosaic"
  let video = BEM.add_element root "video"

  let top_app_bar_contextual = BEM.add_modifier Top_app_bar.CSS.root "contextual-action"
end
