open Js_of_ocaml
open Components

let ( % ) f g x = f (g x)

module Attr = struct
  let keep_aspect_ratio = "data-keep-aspect-ratio"
  let aspect_ratio = "data-aspect-ratio"
  let width = "data-width"
  let height = "data-height"
  let left = "data-left"
  let top = "data-top"
end

type line =
  { is_vertical : bool (* Is line vertical *)
  ; is_multiple : bool (* Multiple intersection detected *)
  ; is_center : bool
  ; x : int
  ; y : int
  } [@@deriving show]

type resize_direction =
  | Top_left
  | Top_right
  | Bottom_left
  | Bottom_right
  | Top
  | Bottom
  | Left
  | Right

type line_align_direction =
  | Horizontal_Top
  | Horizontal_Center
  | Horizontal_Bottom
  | Vertical_Left
  | Vertical_Center
  | Vertical_Right
  | Nill

type t =
  { x : int
  ; y : int
  ; w : int
  ; h : int
  }

let empty =
  { x = 0
  ; y = 0
  ; w = 0
  ; h = 0
  }

let show { x; y; w; h } =
  Printf.sprintf "x=%d, y=%d, w=%d, h=%d"
    x y w h

let compare (a : t) (b : t) =
  let c = compare a.x b.x in
  if c <> 0 then c
  else (let c = compare a.y b.y in
        if c <> 0 then c
        else (let c = compare a.w b.w in
              if c <> 0 then c
              else compare a.h b.h))

let equal (a : t) (b : t) =
  a.x = b.x && a.y = b.y && a.w = b.w && a.h = b.h

(** Checks if two elements collide, returns [true] if so and [false] otherwise *)
let collides (pos1 : t) (pos2 : t) =
  if (pos1.x + pos1.w <= pos2.x) then false
  else if (pos1.x >= pos2.x + pos2.w) then false
  else if (pos1.y + pos1.h <= pos2.y) then false
  else if (pos1.y >= pos2.y + pos2.h) then false
  else true

(** Returns first collision *)
let collision_map
    ~(f : 'a -> t)
    (pos : 'a)
    (l : 'a list) =
  let rec aux = function
    | [] -> None
    | hd :: tl ->
      if collides (f pos) (f hd)
      then Some hd
      else aux tl
  in
  aux l

(** Returns all collisions *)
let collisions_map
    ~(f : 'a -> t)
    (item : 'a)
    (l : 'a list) =
  List.fold_left (fun acc (x : 'a) ->
      if collides (f item) (f x)
      then x :: acc
      else acc) [] l

(** Checks if element collides with other elements *)
let has_collision_map ~f x (l : 'a list) =
  match collision_map ~f x l with
  | None -> false | Some _ -> true

let collision x l = collision_map ~f:(fun x -> x) x l

let collisions x l = collisions_map ~f:(fun x -> x) x l

let has_collision x l = has_collision_map ~f:(fun x -> x) x l

(** Changes top and left coordinates to correspond parent dimentions *)
let fix_xy par_w par_h (p : t) =
  let x = if p.x < 0 then 0 else if p.x + p.w > par_w then par_w - p.w else p.x in
  let y =
    match par_h with
    | None -> if p.y < 0 then 0 else p.y
    | Some ph -> if p.y < 0 then 0 else if p.y + p.h > ph then ph - p.h else p.y
  in
  { p with x; y }

(** Changes width to correspond provided constraints *)
let fix_w ?max_w ?(min_w = 1) par_w (p : t) =
  let w = match max_w with
    | Some max ->
      if p.w > max then max
      else if p.w < min_w then min_w
      else p.w
    | None -> if p.w < min_w then min_w else p.w
  in
  let w = if p.x + w > par_w then par_w - p.x else w in
  { p with w }

(** Changes height to correspond provided constraints *)
let fix_h ?max_h ?(min_h = 1) par_h (p : t) =
  let h = match max_h with
    | Some max -> if p.h > max then max else if p.h < min_h then min_h else p.h
    | None     -> if p.h < min_h then min_h else p.h
  in
  let h = match par_h with
    | Some ph -> if p.y + h > ph then ph - p.y else h
    | None    -> h
  in
  { p with h }

(** Changes width and height to correspond provided constraints *)
let fix_wh ?max_w ?min_w ?max_h ?min_h par_w par_h =
  (fix_h ?max_h ?min_h par_h) % (fix_w ?max_w ?min_w par_w)

(** Changes width and height to correspond provided aspect *)
let fix_aspect (p : t) (aspect : int * int) =
  let w =
    if p.w mod (fst aspect) <> 0 then
      let w = (p.w / (fst aspect)) * (fst aspect) in
      if w = 0 then (fst aspect)  else w
    else p.w
  in
  let h =
    if p.h mod (snd aspect) <> 0 then
      let h = (p.h / (snd aspect)) * (snd aspect) in
      if h = 0 then (snd aspect) else h
    else p.h
  in
  let sw = w / (fst aspect) in
  let sh = h / (snd aspect) in
  let w, h =
    if sw > sh
    then (fst aspect) * sh, h
    else w, (snd aspect) * sw
  in
  { p with w; h }

let apply_to_element ?min_size (pos : t) (elt : #Dom_html.element Js.t) =
  let apply_xw () =
    elt##.style##.width := Utils.px_js pos.w;
    elt##.style##.left := Utils.px_js pos.x in
  let apply_yh () =
    elt##.style##.height := Utils.px_js pos.h;
    elt##.style##.top := Utils.px_js pos.y in
  match min_size with
  | None -> apply_xw (); apply_yh ()
  | Some size ->
    if pos.w >= size
    then apply_xw ();
    if pos.h >= size
    then apply_yh ()

let of_element (elt : #Dom_html.element Js.t) =
  { x = elt##.offsetLeft
  ; y = elt##.offsetTop
  ; w = elt##.offsetWidth
  ; h = elt##.offsetHeight
  }

let to_client_rect (t : t) : Dom_html.clientRect Js.t =
  object%js
    val top = float_of_int t.y
    val left = float_of_int t.x
    val right = float_of_int @@ t.x + t.w
    val bottom = float_of_int @@ t.y + t.h
    val width = Js.def @@ float_of_int t.w
    val height = Js.def @@ float_of_int t.h
  end

let of_client_rect (rect : Dom_html.clientRect Js.t) : t =
  { x = int_of_float rect##.left
  ; y = int_of_float rect##.top
  ; w = Js.Optdef.case rect##.width (fun () -> 0) int_of_float
  ; h = Js.Optdef.case rect##.height (fun () -> 0) int_of_float
  }

let default_aspect_ratio = 1.

let string_of_float = Printf.sprintf "%g"

let get_int_attribute (elt : #Dom_html.element Js.t) attr : int =
  match Element.get_attribute elt attr with
  | None -> 0
  | Some x ->
    match int_of_string_opt x with
    | None -> 0
    | Some x -> x

let get_original_width (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.width

let get_original_height (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.height

let get_original_left (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.left

let get_original_top (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.top

let get_original_aspect_ratio (elt : #Dom_html.element Js.t) : float option =
  match Element.get_attribute elt Attr.aspect_ratio with
  | Some x -> Some (float_of_string x)
  | None ->
    let w, h = get_original_width elt, get_original_height elt in
    if w = 0 || h = 0
    then None
    else
      let ar = (float_of_int w) /. (float_of_int h) in
      Element.set_attribute elt Attr.aspect_ratio (string_of_float ar);
      Some ar

(* min_distance - pixels
     return: minimum distance of several lines of one align *)
let line_find_closest_align_value
    (item : Dom_html.element Js.t)
    (pos : t)
    (items : Dom_html.element Js.t list)
    min_distance
    line_align_val =
  let rec count_aligns line_align_val distance = function
    | [] -> distance
    | hd :: tl ->
      let icompare = of_element hd in
      let distance =
        if Element.equal item hd
        then count_aligns line_align_val distance tl (*distance*) else
          match line_align_val with
          | Horizontal_Top ->
            let dist1 = pos.y - icompare.y in
            let dist2 = pos.y - icompare.y - icompare.h / 2 in
            let dist3 = pos.y - icompare.y - icompare.h in
            if (abs dist1 < min_distance)
            && (abs distance > abs dist1)
            && (abs dist1 <= abs dist2)
            && (abs dist1 <= abs dist3)
            then dist1
            else if (abs dist2 < min_distance)
                 && (abs distance > abs dist2)
                 && (abs dist2 <= abs dist1)
                 && (abs dist2 <= abs dist3)
            then dist2
            else if (abs dist3 < min_distance)
                 && (abs distance > abs dist3)
                 && (abs dist3 <= abs dist1)
                 && (abs dist3 <= abs dist2)
            then dist3
            else distance
          | Horizontal_Center ->
            let dist1 = pos.y + pos.h / 2 - icompare.y in
            let dist2 = pos.y + pos.h / 2 - icompare.y - icompare.h / 2 in
            let dist3 = pos.y + pos.h / 2 - icompare.y - icompare.h in
            if (abs dist1 < min_distance)
            && (abs distance > abs dist1)
            && (abs dist1 <= abs dist2)
            && (abs dist1 <= abs dist3)
            then dist1
            else if (abs dist2 < min_distance)
                 && (abs distance > abs dist2)
                 && (abs dist2 <= abs dist1)
                 && (abs dist2 <= abs dist3)
            then dist2
            else if (abs dist3 < min_distance)
                 && (abs distance > abs dist3)
                 && (abs dist3 <= abs dist1)
                 && (abs dist3 <= abs dist2)
            then dist3
            else distance
          | Horizontal_Bottom ->
            let dist1 = pos.y + pos.h - icompare.y in
            let dist2 = pos.y + pos.h - icompare.y - icompare.h / 2 in
            let dist3 = pos.y + pos.h - icompare.y - icompare.h in
            if (abs dist1 < min_distance)
            && (abs distance > abs dist1)
            && (abs dist1 <= abs dist2)
            && (abs dist1 <= abs dist3)
            then dist1
            else if (abs dist2 < min_distance)
                 && (abs distance > abs dist2)
                 && (abs dist2 <= abs dist1)
                 && (abs dist2 <= abs dist3)
            then dist2
            else if (abs dist3 < min_distance)
                 && (abs distance > abs dist3)
                 && (abs dist3 <= abs dist1)
                 && (abs dist3 <= abs dist2)
            then dist3
            else distance
          | Vertical_Left ->
            let dist1 = pos.x - icompare.x in
            let dist2 = pos.x - icompare.x - icompare.w / 2 in
            let dist3 = pos.x - icompare.x - icompare.w in
            if (abs dist1 < min_distance)
            && (abs distance > abs dist1)
            && (abs dist1 <= abs dist2)
            && (abs dist1 <= abs dist3)
            then dist1
            else if (abs dist2 < min_distance)
                 && (abs distance > abs dist2)
                 && (abs dist2 <= abs dist1)
                 && (abs dist2 <= abs dist3)
            then dist2
            else if (abs dist3 < min_distance)
                 && (abs distance > abs dist3)
                 && (abs dist3 <= abs dist1)
                 && (abs dist3 <= abs dist2)
            then dist3
            else distance
          | Vertical_Center ->
            let dist1 = pos.x + pos.w / 2 - icompare.x in
            let dist2 = pos.x + pos.w / 2 - icompare.x - icompare.w / 2 in
            let dist3 = pos.x + pos.w / 2 - icompare.x - icompare.w in
            if (abs dist1 < min_distance)
            && (abs distance > abs dist1)
            && (abs dist1 <= abs dist2)
            && (abs dist1 <= abs dist3)
            then dist1
            else if (abs dist2 < min_distance)
                 && (abs distance > abs dist2)
                 && (abs dist2 <= abs dist1)
                 && (abs dist2 <= abs dist3)
            then dist2
            else if (abs dist3 < min_distance)
                 && (abs distance > abs dist3)
                 && (abs dist3 <= abs dist1)
                 && (abs dist3 <= abs dist2)
            then dist3
            else distance
          | Vertical_Right ->
            let dist1 = pos.x + pos.w - icompare.x in
            let dist2 = pos.x + pos.w - icompare.x - icompare.w / 2 in
            let dist3 = pos.x + pos.w - icompare.x - icompare.w in
            if (abs dist1 < min_distance)
            && (abs distance > abs dist1)
            && (abs dist1 <= abs dist2)
            && (abs dist1 <= abs dist3)
            then dist1
            else if (abs dist2 < min_distance)
                 && (abs distance > abs dist2)
                 && (abs dist2 <= abs dist1)
                 && (abs dist2 <= abs dist3)
            then dist2
            else if (abs dist3 < min_distance)
                 && (abs distance > abs dist3)
                 && (abs dist3 <= abs dist1)
                 && (abs dist3 <= abs dist2)
            then dist3
            else distance
          | Nill -> distance
      in
      count_aligns line_align_val distance tl
  in
  count_aligns line_align_val (min_distance + 1) items

(* min_distance - pixels
   return: counts of align of selected type in min_distance interval *)
let line_align_count
    (item : Dom_html.element Js.t)
    (pos : t)
    (items : Dom_html.element Js.t list)
    min_distance
    line_align_val =

  let rec count_aligns line_align_val counts = function
    | [] -> counts
    | hd :: tl ->
      let icompare = of_element hd in
      if Element.equal item hd
      then count_aligns line_align_val counts tl (*counts*)
      else
        let counts =
          if (line_align_val = Horizontal_Top
              && (abs (pos.y - icompare.y) < min_distance
                  || abs (pos.y - icompare.y - icompare.h / 2) < min_distance
                  || abs (pos.y - icompare.y - icompare.h) < min_distance))
          || (line_align_val = Horizontal_Center
              && (abs (pos.y + pos.h / 2 - icompare.y) < min_distance
                  || abs (pos.y + pos.h / 2 - icompare.y - icompare.h / 2) < min_distance
                  || abs (pos.y + pos.h / 2 - icompare.y - icompare.h) < min_distance))
          || (line_align_val = Horizontal_Bottom
              && (abs (pos.y + pos.h - icompare.y) < min_distance
                  || abs (pos.y + pos.h - icompare.y - icompare.h / 2) < min_distance
                  || abs (pos.y + pos.h - icompare.y - icompare.h) < min_distance))
          || (line_align_val = Vertical_Left
              && (abs (pos.x - icompare.x) < min_distance
                  || abs (pos.x - icompare.x - icompare.w / 2) < min_distance
                  || abs (pos.x - icompare.x - icompare.w) < min_distance))
          || (line_align_val = Vertical_Center
              && (abs (pos.x + pos.w / 2 - icompare.x) < min_distance
                  || abs (pos.x + pos.w / 2 - icompare.x - icompare.w / 2) < min_distance
                  || abs (pos.x + pos.w / 2 - icompare.x - icompare.w) < min_distance))
          || (line_align_val = Vertical_Right
              && (abs (pos.x + pos.w - icompare.x) < min_distance
                  || abs (pos.x + pos.w - icompare.x - icompare.w / 2) < min_distance
                  || abs (pos.x + pos.w - icompare.x - icompare.w) < min_distance))
          then succ counts
          else counts
        in
        count_aligns line_align_val counts tl
  in
  count_aligns line_align_val 0 items

(* return: direction, count aligns (0 = none align lines),
   closest line distance (if distance > min_distance = no find lines) *)
let horizontal_lines_aligned_list_for_move_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list) =
  let ret_list =
    [ Horizontal_Top,
      line_align_count item pos items min_distance Horizontal_Top,
      line_find_closest_align_value item pos items min_distance Horizontal_Top
    ; Horizontal_Center,
      line_align_count item pos items min_distance Horizontal_Center,
      line_find_closest_align_value item pos items min_distance Horizontal_Center
    ; Horizontal_Bottom,
      line_align_count item pos items min_distance Horizontal_Bottom,
      line_find_closest_align_value item pos items min_distance Horizontal_Bottom
    ] in
  ret_list

let horizontal_lines_aligned_list_for_resize_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (direction : resize_direction) =
  let ret_list =
    match direction with
    | Top_left ->
      [ Horizontal_Top,
        line_align_count item pos items min_distance Horizontal_Top,
        line_find_closest_align_value item pos items min_distance Horizontal_Top
      ]
    | Top_right ->
      [ Horizontal_Top,
        line_align_count item pos items min_distance Horizontal_Top,
        line_find_closest_align_value item pos items min_distance Horizontal_Top
      ]
    | Bottom_left ->
      [ Horizontal_Bottom,
        line_align_count item pos items min_distance Horizontal_Bottom,
        line_find_closest_align_value item pos items min_distance Horizontal_Bottom
      ]
    | Bottom_right ->
      [ Horizontal_Bottom,
        line_align_count item pos items min_distance Horizontal_Bottom,
        line_find_closest_align_value item pos items min_distance Horizontal_Bottom
      ]
    | Top ->
      [ Horizontal_Top,
        line_align_count item pos items min_distance Horizontal_Top,
        line_find_closest_align_value item pos items min_distance Horizontal_Top
      ]
    | Bottom ->
      [ Horizontal_Bottom,
        line_align_count item pos items min_distance Horizontal_Bottom,
        line_find_closest_align_value item pos items min_distance Horizontal_Bottom
      ]
    | Left ->
      [ Horizontal_Center,
        line_align_count item pos items min_distance Horizontal_Center,
        line_find_closest_align_value item pos items min_distance Horizontal_Center
      ]
    | Right ->
      [ Horizontal_Center,
        line_align_count item pos items min_distance Horizontal_Center,
        line_find_closest_align_value item pos items min_distance Horizontal_Center
      ]
  in
  ret_list

let vertical_lines_aligned_list_for_move_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list) =
  let ret_list =
    [ Vertical_Left,
      line_align_count item pos items min_distance Vertical_Left,
      line_find_closest_align_value item pos items min_distance Vertical_Left
    ; Vertical_Center,
      line_align_count item pos items min_distance Vertical_Center,
      line_find_closest_align_value item pos items min_distance Vertical_Center
    ; Vertical_Right,
      line_align_count item pos items min_distance Vertical_Right,
      line_find_closest_align_value item pos items min_distance Vertical_Right
    ] in
  ret_list

let vertical_lines_aligned_list_for_resize_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (direction : resize_direction) =
  let ret_list = match direction with
    | Top_left ->
      [ Vertical_Left,
        line_align_count item pos items min_distance Vertical_Left,
        line_find_closest_align_value item pos items min_distance Vertical_Left
      ]
    | Top_right ->
      [ Vertical_Right,
        line_align_count item pos items min_distance Vertical_Right,
        line_find_closest_align_value item pos items min_distance Vertical_Right
      ]
    | Bottom_left ->
      [ Vertical_Left,
        line_align_count item pos items min_distance Vertical_Left,
        line_find_closest_align_value item pos items min_distance Vertical_Left
      ]
    | Bottom_right ->
      [ Vertical_Right,
        line_align_count item pos items min_distance Vertical_Right,
        line_find_closest_align_value item pos items min_distance Vertical_Right
      ]
    | Top ->
      [ Vertical_Center,
        line_align_count item pos items min_distance Vertical_Center,
        line_find_closest_align_value item pos items min_distance Vertical_Center
      ]
    | Bottom ->
      [ Vertical_Center,
        line_align_count item pos items min_distance Vertical_Center,
        line_find_closest_align_value item pos items min_distance Vertical_Center
      ]
    | Left ->
      [ Vertical_Left,
        line_align_count item pos items min_distance Vertical_Left,
        line_find_closest_align_value item pos items min_distance Vertical_Left
      ]
    | Right ->
      [ Vertical_Right,
        line_align_count item pos items min_distance Vertical_Right,
        line_find_closest_align_value item pos items min_distance Vertical_Right
      ] in
  ret_list

let get_snap (item : Dom_html.element Js.t) coord min_distance items =
  let rec aux snap snap_min_delta = function
    | [] -> snap
    | (_, aligns_count, distance) :: tl ->
      let snap_min_delta =
        if aligns_count > 0 && abs distance < abs snap_min_delta
        then distance else snap_min_delta in
      let snap =
        if abs snap_min_delta <= min_distance
        then coord - snap_min_delta else snap in
      aux snap snap_min_delta tl in
  aux coord (min_distance + 1) items

let get_item_snap_y (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list) =
  let snap_list = horizontal_lines_aligned_list_for_move_action item pos min_distance items in
  get_snap item pos.y min_distance snap_list

let get_item_snap_x (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list) =
  let snap_list = vertical_lines_aligned_list_for_move_action item pos min_distance items in
  get_snap item pos.x min_distance snap_list

let get_item_snap_position_for_move_action
    (item : Dom_html.element Js.t)
    pos
    min_distance
    (items : Dom_html.element Js.t list) =
  { x = get_item_snap_x item pos min_distance items
  ; y = get_item_snap_y item pos min_distance items
  ; w = pos.w
  ; h = pos.h
  }

let get_item_snap_position_for_resize_action
    (item : Dom_html.element Js.t)
    pos
    min_distance
    (items : Dom_html.element Js.t list)
    (direction : resize_direction) =
  match direction with
  | Top_left ->
    let snap_list_x = [
      Vertical_Left,
      line_align_count item pos items min_distance Vertical_Left,
      line_find_closest_align_value item pos items min_distance Vertical_Left]
    in
    let snap_list_y = [
      Horizontal_Top,
      line_align_count item pos items min_distance Horizontal_Top,
      line_find_closest_align_value item pos items min_distance Horizontal_Top]
    in
    (*let _ = Printf.printf "top left resize\n" in*)
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x - (get_snap item pos.x min_distance snap_list_x) + pos.w
    ; h = pos.y - (get_snap item pos.y min_distance snap_list_y) + pos.h
    }
  | Top_right ->
    let snap_list_x = [
      Vertical_Right,
      line_align_count item pos items min_distance Vertical_Right,
      line_find_closest_align_value item pos items min_distance Vertical_Right]
    in
    let snap_list_y = [
      Horizontal_Top,
      line_align_count item pos items min_distance Horizontal_Top,
      line_find_closest_align_value item pos items min_distance Horizontal_Top]
    in
    { x = pos.x (*get_snap item pos.x min_distance snap_list_x *)
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = (get_snap item pos.x min_distance snap_list_x) - pos.x + pos.w
    ; h = pos.y - (get_snap item pos.y min_distance snap_list_y) + pos.h
    }
  | Bottom_left ->
    let snap_list_x = [
      Vertical_Left,
      line_align_count item pos items min_distance Vertical_Left,
      line_find_closest_align_value item pos items min_distance Vertical_Left]
    in
    let snap_list_y = [
      Horizontal_Bottom,
      line_align_count item pos items min_distance Horizontal_Bottom,
      line_find_closest_align_value item pos items min_distance Horizontal_Bottom]
    in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = pos.y (*get_snap item pos.y min_distance snap_list_y *)
    ; w = pos.x - (get_snap item pos.x min_distance snap_list_x) + pos.w
    ; h = (get_snap item pos.y min_distance snap_list_y) - pos.y + pos.h
    }
  | Bottom_right ->
    let snap_list_x = [
      Vertical_Right,
      line_align_count item pos items min_distance Vertical_Right,
      line_find_closest_align_value item pos items min_distance Vertical_Right]
    in
    let snap_list_y = [
      Horizontal_Bottom,
      line_align_count item pos items min_distance Horizontal_Bottom,
      line_find_closest_align_value item pos items min_distance Horizontal_Bottom]
    in
    { x = pos.x (*get_snap item pos.x min_distance snap_list_x *)
    ; y = pos.y (*get_snap item pos.y min_distance snap_list_y *)
    ; w = (get_snap item pos.x min_distance snap_list_x) - pos.x + pos.w
    ; h = (get_snap item pos.y min_distance snap_list_y) - pos.y + pos.h
    }
  | Top ->
    (* not tested *)
    let snap_list_x = [
      Vertical_Center,
      line_align_count item pos items min_distance Vertical_Center,
      line_find_closest_align_value item pos items min_distance Vertical_Center]
    in
    let snap_list_y = [
      Horizontal_Top,
      line_align_count item pos items min_distance Horizontal_Top,
      line_find_closest_align_value item pos items min_distance Horizontal_Top]
    in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x - (get_snap item pos.x min_distance snap_list_x) + pos.w
    ; h = pos.y - (get_snap item pos.y min_distance snap_list_y) + pos.h
    }
  | Bottom ->
    (* not tested *)
    let snap_list_x = [
      Vertical_Center,
      line_align_count item pos items min_distance Vertical_Center,
      line_find_closest_align_value item pos items min_distance Vertical_Center]
    in
    let snap_list_y = [
      Horizontal_Bottom,
      line_align_count item pos items min_distance Horizontal_Bottom,
      line_find_closest_align_value item pos items min_distance Horizontal_Bottom]
    in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x - (get_snap item pos.x min_distance snap_list_x) + pos.w
    ; h = pos.y - (get_snap item pos.y min_distance snap_list_y) + pos.h
    }
  | Left ->
    (* not tested *)
    let snap_list_x = [
      Vertical_Left,
      line_align_count item pos items min_distance Vertical_Left,
      line_find_closest_align_value item pos items min_distance Vertical_Left]
    in
    let snap_list_y = [
      Horizontal_Center,
      line_align_count item pos items min_distance Horizontal_Center,
      line_find_closest_align_value item pos items min_distance Horizontal_Center]
    in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x - (get_snap item pos.x min_distance snap_list_x) + pos.w
    ; h = pos.y - (get_snap item pos.y min_distance snap_list_y) + pos.h
    }
  | Right ->
    (* not tested *)
    let snap_list_x = [
      Vertical_Right,
      line_align_count item pos items min_distance Vertical_Right,
      line_find_closest_align_value item pos items min_distance Vertical_Right]
    in
    let snap_list_y = [
      Horizontal_Center,
      line_align_count item pos items min_distance Horizontal_Center,
      line_find_closest_align_value item pos items min_distance Horizontal_Center]
    in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x - (get_snap item pos.x min_distance snap_list_x) + pos.w
    ; h = pos.y - (get_snap item pos.y min_distance snap_list_y) + pos.h
    }

(* glue lines to its item *)
let get_snap_lines
    (item : Dom_html.element Js.t)
    (pos : t)
    (items : Dom_html.element Js.t list)
    min_distance
    (action : [`Resize of resize_direction | `Move]) =
  let snap_list_v =
    match action with
    | `Move ->
      vertical_lines_aligned_list_for_move_action item pos
        min_distance items
    | `Resize direction ->
      vertical_lines_aligned_list_for_resize_action item pos
        min_distance items direction
  in
  let snap_list_h =
    match action with
    | `Move ->
      horizontal_lines_aligned_list_for_move_action item pos
        min_distance items
    | `Resize direction ->
      horizontal_lines_aligned_list_for_resize_action item pos
        min_distance items direction
  in
  let rec create_lines_v_list (action : [`Resize of resize_direction | `Move])
      acc = function
    | [] -> acc
    | (direction, aligns_count, distance) :: tl ->
      let acc =
        if aligns_count > 0
        then
          let line_ret =
            { is_vertical = true
            ; is_multiple = aligns_count > 1
            ; is_center = direction = Vertical_Center
            ; x = if direction = Vertical_Left
                then pos.x
                else if direction = Vertical_Center
                then pos.x + pos.w / 2
                else if direction = Vertical_Right
                then pos.x + pos.w
                else 0
            ; y = 0
            } in
          line_ret :: acc
        else acc in
      create_lines_v_list action acc tl in

  let rec create_lines_h_list (action : [`Resize of resize_direction | `Move])
      acc = function
    | [] -> acc
    | (direction, aligns_count, distance) :: tl ->
      let acc =
        if aligns_count > 0
        then
          let line_ret =
            { is_vertical = false
            ; is_multiple = aligns_count > 1
            ; is_center = direction = Horizontal_Center
            ; x = 0
            ; y = if direction = Horizontal_Top
                then pos.y
                else if direction = Horizontal_Center
                then pos.y + pos.h / 2
                else if direction = Horizontal_Bottom
                then pos.y + pos.h
                else 0
            } in
          line_ret :: acc
        else acc in
      create_lines_h_list action acc tl in
  create_lines_v_list action [] snap_list_v
  @ create_lines_h_list action [] snap_list_h

(* FIXME remove *)
let is_item_collide_others
    (position : t)
    (item : Dom_html.element Js.t)
    (items : Dom_html.element Js.t list) =
  let rec _is_item_collide_others (is_collide:bool) (position : t)
      (item : Dom_html.element Js.t) = function
    | [] -> is_collide
    | hd :: tl ->
      let otherpos = of_element hd in
      if (collides position otherpos) && (item <> hd)
      then true
      else _is_item_collide_others is_collide position item tl in
  _is_item_collide_others false position item items

let position_not_collide_others
    (original_position : t)
    (position : t)
    (item : Dom_html.element Js.t)
    (items : Dom_html.element Js.t list) =
  if is_item_collide_others position item items
  then original_position
  else position

(* FIXME remove*)
let global_saved_original_position = ref {x=0;y=0;h=0;w=0}
let global_saved_position_previous = ref {x=0;y=0;h=0;w=0}

let adjust ?aspect_ratio
    ?(snap_lines = true)
    ?(collisions = false)
    ~(action : [`Resize of resize_direction | `Move])
    ~(original_position : t)
    ~(position : t)
    ~(siblings : Dom_html.element Js.t list)
    ~(parent_size : int * int)
    (item : Dom_html.element Js.t) : t * line list =
  let min_distance = 12 in
  let position =
    if position.x < 0 && position.y < 0
    then { position with x = 0 ; y = 0 }
    else if position.x < 0
         && position.y > (snd parent_size) - position.h
    then { position with x = 0 ; y = (snd parent_size) - position.h }
    else if position.x > (fst parent_size) - position.w
         && position.y < 0
    then { position with x = (fst parent_size) - position.w
                       ; y = 0 }
    else if position.x > (fst parent_size) - position.w
         && position.y > (snd parent_size) - position.h
    then { position with x = (fst parent_size) - position.w
                       ; y = (snd parent_size) - position.h }
    else if position.x < 0
    then { position with x = 0 }
    else if position.y < 0
    then { position with y = 0 }
    else if position.x + position.w >= (fst parent_size)
    then { position with x = (fst parent_size) - position.w }
    else if position.y + position.h >= (snd parent_size)
    then { position with y = (snd parent_size) - position.h }
    else position
  in
  let position_snaped = match snap_lines, action with
    | false, _ -> position
    | true, `Move ->
      get_item_snap_position_for_move_action item position
        min_distance siblings
    | true, `Resize resz ->
      get_item_snap_position_for_resize_action item position
        min_distance siblings resz
  in
  (* FIXME remove *)
  let last_pos =
    if !global_saved_original_position <> original_position
    then begin
      global_saved_original_position:=original_position;
      global_saved_position_previous:=original_position;
      original_position
    end
    else !global_saved_position_previous in
  let position_not_collide =
    if collisions
    then position_not_collide_others last_pos position_snaped item siblings
    else position_snaped in
  let snap_lines =
    if snap_lines
    then get_snap_lines item position_not_collide siblings min_distance action
    else [] in
  (* FIXME remove *)
  global_saved_position_previous := position_not_collide;
  position_not_collide, snap_lines

let scale
    ~(original_parent_size : int * int)
    ~(parent_size : int * int)
    (position : t) : t =
  let pos = if (fst parent_size) <> 0 then
      { x = position.x * (fst original_parent_size) / (fst parent_size)
      ; y = position.y * (fst original_parent_size) / (fst parent_size)
      ; w = position.w * (fst original_parent_size) / (fst parent_size)
      ; h = position.h * (fst original_parent_size) / (fst parent_size)
      }
    else position
  in
  pos

let find_spare ?(compare : (t -> t -> int) option)
    ?(aspect : (int * int) option)
    ?min_w ?min_h ?max_w ?max_h
    ~(siblings : t list)
    ~parent_size
    (x, y) =
  let (w, h) = parent_size in
  let pos = { x; y; w = 1; h = 1 } in
  if has_collision pos siblings
  then None
  else
    let area pos = pos.w * pos.h in
    let cmp =
      match compare with
      | Some f -> f
      | None ->
        (fun new_pos old_pos ->
           match aspect with
           | Some a ->
             let nasp = fix_aspect new_pos a in
             let oasp = fix_aspect old_pos a in
             let narea = area nasp in
             let oarea = area oasp in
             Stdlib.compare narea oarea
           | None ->
             let new_area = area new_pos in
             let old_area = area old_pos in
             Stdlib.compare new_area old_area) in
    (* FIXME obviously not optimized algorithm *)
    (* get only elements that are on the way to cursor proection to the left/right side *)
    let x_filtered = List.filter (fun i -> pos.y > i.y && pos.y < i.y + i.h) siblings in
    (* get cursor proection to the left side *)
    let l =
      List.filter (fun i -> i.x < pos.x) x_filtered
      |> List.fold_left (fun acc i ->
          if i.x + i.w > acc.x + acc.w
          then i else acc) empty
      |> (fun x -> x.x + x.w) in
    (* get cursor proection to the right side *)
    let r =
      List.filter (fun i -> i.x > pos.x) x_filtered
      |> List.fold_left (fun acc i ->
          if i.x < acc.x then i else acc)
        { x = w; y = 0; w = 0; h = 0 }
      |> (fun x -> x.x) in
    (* get only elements that are on the way to cursor proection to the top/bottom side *)
    let y_filtered = List.filter
        (fun i ->
           pos.x > i.x
           && pos.x < i.x + i.w
           && i.x + i.w > l
           && i.x < r)
        siblings in
    (* get cursor proection to the top side *)
    let t =
      List.filter (fun i -> i.y < pos.y) y_filtered
      |> List.fold_left (fun acc i ->
          if i.y + i.h > acc.y + acc.h
          then i else acc) empty
      |> (fun x -> x.y + x.h) in
    (* get cursor proection to the bottom side *)
    let b =
      List.filter (fun i -> i.y > pos.y) y_filtered
      |> List.fold_left (fun acc i -> if i.y < acc.y then i else acc)
        { x = 0; y = h; w = 0; h = 0}
      |> (fun x -> x.y) in
    (* get available x points *)
    (* FIXME obviously we don't need to iterate over all items *)
    let (xs : int list) =
      List.fold_left (fun acc i ->
          let join = fun x lst -> if x >= l && x <= r then x :: lst else lst in
          let acc  = join i.x acc |> join (i.x + i.w) in
          acc) [] siblings
      |> (fun x -> l :: x @ [r])
      |> List.sort_uniq (Pervasives.compare) in
    (* get available y points *)
    (* FIXME obviously we don't need to iterate over all items *)
    let (ys : int list) =
      List.fold_left (fun acc i ->
          let join = fun y lst -> if y >= t && y <= b then y :: lst else lst in
          let acc  = join i.y acc |> join (i.y + i.h) in
          acc) [] siblings
      |> (fun x -> t :: x @ [b])
      |> List.sort_uniq (Pervasives.compare) in
    (* get biggest non-overlapping rectangle under the cursor *)
    (* FIXME obviously not optimized at all *)
    let a =
      List.fold_left (fun acc x0 ->
          let xs = List.filter (fun i -> i > x0) xs in
          List.fold_left (fun acc x1 ->
              List.fold_left (fun acc y0 ->
                  let ys = List.filter (fun i -> i > y0) ys in
                  List.fold_left (fun acc y1 ->
                      let (new_pos : t) =
                        { x = x0
                        ; y = y0
                        ; w = x1 - x0
                        ; h = y1 - y0
                        } in
                      (* new rect must be the biggest one available,
                       * it must not overlap with other rects,
                       * it must be under the mouse cursor *)
                      if (not @@ has_collision new_pos siblings) && collides new_pos pos
                      then
                        let p = fix_wh ?max_w ?max_h ?min_h ?min_w w (Some h) new_pos in
                        let p =
                          match aspect with
                          | Some asp -> fix_aspect p asp
                          | None -> new_pos in
                        let cx = pos.x - new_pos.x in
                        let cy = pos.y - new_pos.y in
                        let cp = fix_xy new_pos.w (Some new_pos.h)
                            { pos with x = cx; y = cy } in
                        let p = fix_xy new_pos.w (Some new_pos.h)
                            { p with x = cp.x - (p.w / 2)
                                   ; y = cp.y - (p.h / 2)
                            } in
                        let p = { p with x = p.x + new_pos.x
                                       ; y = p.y + new_pos.y } in
                        match (cmp p acc),
                              p.x + p.w > w,
                              p.y + p.h > h,
                              p.w > new_pos.w,
                              p.h > new_pos.h with
                        | 1, false, false, false, false -> p
                        | _ -> acc
                      else acc) acc ys) acc ys) acc xs)
        empty xs
    in
    if equal a empty then None else Some a
