open Containers
open Components
open Pipeline_js

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
  }

let min_size_to_yojson x =
  Common.Json.(
    Option.to_yojson (Pair.to_yojson Int.to_yojson Int.to_yojson) x)

let min_size_of_yojson json =
  Common.Json.(
    Option.of_yojson (Pair.of_yojson Int.of_yojson Int.of_yojson) json)

let equal_wm_item (eq : 'a -> 'a -> bool)
      (a : 'a wm_item) (b : 'a wm_item) : bool =
  eq a.item b.item
  && Widget.equal a.icon b.icon
  && Bool.equal a.unique b.unique
  && Option.equal (Pair.equal (=) (=)) a.min_size b.min_size

let wm_item_to_yojson (f : 'a -> Yojson.Safe.json)
      (x : 'a wm_item) : Yojson.Safe.json =
  `Assoc [ "icon", (icon_to_yojson x.icon)
         ; "name", `String x.name
         ; "unique", `Bool x.unique
         ; "min_size", min_size_to_yojson x.min_size
         ; "item", f x.item ]

let wm_item_of_yojson (f : Yojson.Safe.json -> ('a, string) result)
      (json : Yojson.Safe.json) : ('a wm_item, string) result =
  match json with
  | `Assoc x ->
     let get key = List.Assoc.get_exn ~eq:String.equal key x in
     (try
        Common.Json.(
        let icon = match icon_of_yojson (get "icon") with
          | Ok x -> x
          | Error e -> failwith e in
        let name = match String.of_yojson (get "name") with
          | Ok x -> x
          | Error e -> failwith e in
        let unique = match Bool.of_yojson (get "unique") with
          | Ok x -> x
          | Error e -> failwith e in
        let min_size = match min_size_of_yojson (get "min_size") with
          | Ok x -> x
          | Error e -> failwith e in
        let item = match f (get "item") with
          | Ok x -> x
          | Error e -> failwith e in
        Ok { icon; name; unique; min_size; item })
      with Failure s -> Error s
         | e -> raise e)
     | _ -> Error "wm_item_of_yojson: must be a json object"

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

  type item
  type layout_item = string * item
  type t = item wm_item

  val max_layers : int

  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> (t, string) result
  val equal : t -> t -> bool
  val to_layout_item : t -> layout_item
  val of_layout_item : layout_item -> t
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
