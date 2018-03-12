open Containers
open Components

type add_candidate =
  { icon : string
  ; typ  : string
  ; name : string
  } [@@deriving yojson]

type 'a item_props =
  { widget  : Widget.widget
  ; actions : 'a item_props_action list
  }
and 'a item_props_action =
  { label    : string
  ; on_click : (string * 'a) Dynamic_grid.Item.t -> unit
  }

type editor_config =
  { show_grid_lines : bool
  }

type action =
  { icon   : string
  ; name   : string
  }

module type Item = sig

  type item
  type t = string * item

  val create_item      : add_candidate -> item
  val pos_of_item      : item -> Wm.position
  val layer_of_item    : item -> int
  val layers_of_t_list : t list -> int list
  val update_pos       : item -> Wm.position -> item
  val update_layer     : item -> int -> item

  val max_layers       : int
  val add_candidates   : add_candidate list
  val make_item_name   : add_candidate -> int -> string
  val make_item_props  : (string * item) Dynamic_grid.Item.t -> item item_props

end
