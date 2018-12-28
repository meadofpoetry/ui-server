open Containers
open Components

type icon = Widget.t

let icon_to_yojson w =
  `String w#outer_html
let icon_of_yojson = function
  | `String s ->
     Js_of_ocaml.(
      let div = Dom_html.(createDiv document) in
      div##.innerHTML := Js.string s;
      let elt : Dom_html.element Js.t =
        Js.Opt.get div##.firstChild (fun () -> assert false)
        |> Js.Unsafe.coerce in
      Ok (Widget.create elt))
  | _ -> Error "bad json"
let equal_icon x y =
  Equal.physical x#root y#root

type 'a wm_item =
  { icon : icon
  ; name : string
  ; unique : bool
  ; min_size : (int * int) option
  ; item : 'a
  } [@@deriving yojson, eq]

type item_properties_action =
  { label : string
  ; on_click : unit -> unit
  }
type item_properties =
  { widget : Widget.t
  ; actions : item_properties_action list
  }

type editor_config =
  { show_grid_lines : bool
  }

type action =
  { icon : Widget.t
  ; name : string
  }

module type Item = sig

  type item [@@deriving yojson, eq]
  type layout_item = string * item
  type t = item wm_item [@@deriving yojson, eq]

  val max_layers : int

  val t_to_layout_item : t -> layout_item
  val t_of_layout_item : layout_item -> t
  val to_grid_item : t -> Dynamic_grid.Position.t -> t Dynamic_grid.item
  val position_of_t : t -> Wm.position
  val layer_of_t : t -> int
  val size_of_t : t -> int option * int option
  val layers_of_t_list : t list -> int list
  val update_position : t -> Wm.position -> t
  val update_layer : t -> int -> t
  val update_min_size : t -> t
  val make_item_name : t -> t list -> string
  val make_item_properties : t React.signal -> (t -> unit) -> item_properties

end
