open Containers

include Dynamic_grid_types
include Dynamic_grid_overlay
module Item = Dynamic_grid_item

let to_grid = Dynamic_grid_abstract.to_grid

class ['a] t ~grid ~(items:'a item list) () =
  ['a,'a Item.t,'a item] Dynamic_grid_abstract.t ~get:(fun x -> x) ~grid ~items ()
