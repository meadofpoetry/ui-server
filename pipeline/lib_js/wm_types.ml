open Containers
open Components

type 'a wm_item =
  { icon   : string
  ; name   : string
  ; unique : bool
  ; item   : 'a
  } [@@deriving yojson,eq]

type item_properties_action =
  { label    : string
  ; on_click : unit -> unit
  }
type item_properties =
  { widget  : Widget.widget
  ; actions : item_properties_action list
  }

type editor_config =
  { show_grid_lines : bool
  }

type action =
  { icon   : string
  ; name   : string
  }

module type Item = sig

  type item [@@deriving yojson,eq]
  type layout_item = string * item
  type t = item wm_item [@@deriving yojson,eq]

  val max_layers           : int

  val t_to_layout_item     : t -> layout_item
  val t_of_layout_item     : layout_item -> t
  val position_of_t        : t -> Wm.position
  val layer_of_t           : t -> int
  val size_of_t            : t -> int option * int option
  val layers_of_t_list     : t list -> int list
  val update_position      : t -> Wm.position -> t
  val update_layer         : t -> int -> t
  val make_item_name       : t -> t list -> string
  val make_item_properties : t -> t list -> (t -> unit) -> item_properties

end
