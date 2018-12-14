open Containers
open Components
open Wm_types
open Wm_components

type container_grids =
  { rect : Wm.position
  ; grids : (int * int) list
  }

let pos_absolute_to_relative
      (pos : Wm.position)
      (cont_pos : Wm.position) : Wm.position =
  { left = pos.left - cont_pos.left
  ; right = pos.right - cont_pos.left
  ; top = pos.top - cont_pos.top
  ; bottom = pos.bottom - cont_pos.top
  }

let pos_relative_to_absolute
      (pos : Wm.position)
      (cont_pos : Wm.position) : Wm.position =
  { left = pos.left + cont_pos.left
  ; right = pos.right + cont_pos.left
  ; top = pos.top + cont_pos.top
  ; bottom = pos.bottom + cont_pos.top
  }

let get_bounding_rect (positions : Wm.position list) : Wm.position =
  let open Wm in
  match positions with
  | [] -> { left = 0
          ; right = 0
          ; top = 0
          ; bottom = 0 }
  | hd :: tl ->
     List.fold_left (fun acc (x : Wm.position) ->
         let { left; top; bottom; right } = x in
         let left = min acc.left left in
         let top = min acc.top top in
         let bottom = max acc.bottom bottom in
         let right = max acc.right right in
         { left; top; right; bottom}) hd tl

let get_bounding_rect_and_grids (positions:Wm.position list) =
  let rect       = get_bounding_rect positions in
  let resolution = rect.right - rect.left, rect.bottom - rect.top in
  let positions  =
    List.map (fun x -> pos_absolute_to_relative x rect) positions in
  { rect
  ; grids = Utils.get_grids ~resolution ~positions () }

let resize ~(resolution : int * int)
      ~(to_position : 'a -> Wm.position)
      ~(f : Wm.position -> 'a -> 'a) = function
  | [] -> []
  | l ->
     let grids = get_bounding_rect_and_grids @@ List.map to_position l in
     let rect = grids.rect |> Utils.to_grid_position in
     let nw, nh =
       Utils.resolution_to_aspect (rect.w, rect.h)
       |> Dynamic_grid.Position.correct_aspect
            { x = 0
            ; y = 0
            ; w = fst resolution
            ; h = snd resolution }
       |> (fun p -> p.w, p.h) in
     let w, h =
       if nw > rect.w
       then List.hd grids.grids
       else List.fold_left (fun acc (w, h) ->
                if w > (fst acc) && w <= nw
                then (w,h) else acc) (0,0) grids.grids in
     let cw, rh = nw / w, nh / h in
     let dx = (fst resolution - (w * cw)) / 2 in
     let dy = (snd resolution - (h * rh)) / 2 in
     let apply (item : 'a) : 'a =
       Utils.of_grid_position rect
       |> pos_absolute_to_relative (to_position item)
       |> Wm_items_layer.grid_pos_of_layout_pos
            ~resolution:(rect.w, rect.h) ~cols:w ~rows:h
       |> (fun pos -> Dynamic_grid.Position.(
             { x = (pos.x * cw) + dx
             ; y = (pos.y * rh) + dy
             ; w = pos.w * cw
             ; h = pos.h * rh }))
       |> Utils.of_grid_position
       |> (fun x -> f x item)
     in
     List.map apply l

let resize_container (p : Wm.position) (t : Wm.container wm_item) =
  let resolution = p.right - p.left, p.bottom - p.top in
  let widgets =
    resize ~resolution
      ~to_position:(fun (_, (x : Wm.widget)) -> x.position)
      ~f:(fun pos (s, (x : Wm.widget)) -> s, { x with position = pos })
      t.item.widgets
  in
  { t with item = { t.item with position = p; widgets }}

let resize_layout ~(resolution : int * int) (l : Wm.container wm_item list) =
  let containers =
    resize ~resolution
      ~to_position:(fun (t : Wm.container wm_item) -> t.item.position)
      ~f:resize_container
      l
  in
  containers

module Container_item : Item with type item = Wm.container = struct

  type item = Wm.container [@@deriving yojson, eq]

  type layout_item = string * item

  type t = item wm_item [@@deriving yojson, eq]

  let max_layers = 1

  let update_min_size (t : t) =
    let min_size = match t.item.widgets with
      | [] -> None
      | _  ->
         let positions = List.map (fun (_, (x : Wm.widget)) -> x.position)
                           t.item.widgets in
         let w, h = List.hd (get_bounding_rect_and_grids positions).grids in
         Some (w, h) in
    { t with min_size }

  let t_to_layout_item (t : t) = t.name, t.item

  let t_of_layout_item (k, (v:item)) =
    let t =
      { icon = Icon.SVG.(create_simple Path.contain)#widget
      ; name = k
      ; unique = false
      ; min_size = None
      ; item = v
      }
    in
    update_min_size t

  let to_grid_item (t : t) (pos : Dynamic_grid.Position.t) =
    let widget = Item_info.make_container_info t in
    Dynamic_grid.Item.to_item ~value:t ~widget ~pos ()

  let position_of_t (t : t) = t.item.position

  let layer_of_t _ = 0

  let size_of_t (_ : t) = None, None

  let layers_of_t_list _ = [0]

  let update_position (t : t) (p : Wm.position) =
    let op = t.item.position in
    let nw, nh = p.right - p.left, p.bottom - p.top in
    let ow, oh = op.right - op.left, op.bottom - op.top in
    match (ow <> nw || oh <> nh) && not (List.is_empty t.item.widgets) with
    | true -> resize_container p t
    | false -> { t with item = { t.item with position = p }}

  let update_layer (t : t) _ = t

  let make_item_name (t : t) (other : t list) =
    let rec aux idx other =
      let name = Printf.sprintf "%s #%d" t.name idx in
      match List.partition (fun (x : t) -> String.equal x.name name) other with
      | [], _ -> name
      | _, other -> aux (succ idx) other

    in
    aux 1 other

  let make_item_properties t _ _ =
    Item_properties.make_container_props t

end
