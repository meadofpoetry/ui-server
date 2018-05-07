open Containers

include Dynamic_grid_types
include Dynamic_grid_overlay
module Item = Dynamic_grid_item


type add_error = Collides of Position.t list


let to_grid ?max_col_width ?(min_col_width=1) ?rows ?row_height ?(vertical_compact=false)
            ?(items_margin=0,0) ?(multi_select=false) ?(restrict_move=false) ~cols () =
  { min_col_width; max_col_width; cols; rows;
    row_height; vertical_compact; items_margin;
    multi_select; restrict_move
  }

class ['a] t ~grid ~(items:'a item list) () =
  ['a,'a Item.t,'a item] Dynamic_grid_abstract.t ~get:(fun x -> x) ~grid ~items ()
