open Js_of_ocaml
open Components

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
  | None

type t =
  { x : int
  ; y : int
  ; w : int
  ; h : int
  } [@@deriving show]

let empty =
  { x = 0
  ; y = 0
  ; w = 0
  ; h = 0
  }

let apply_to_element (pos : t) (elt : #Dom_html.element Js.t) =
  let min_size = 20 in (* FIXME *)
  if pos.w >= min_size
  then (
    elt##.style##.width := Utils.px_js pos.w;
    elt##.style##.left := Utils.px_js pos.x);
  if pos.h >= min_size
  then (
    elt##.style##.height := Utils.px_js pos.h;
    elt##.style##.top := Utils.px_js pos.y)

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
          | None -> distance
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
let horizontal_lines_aligned_list (item : Dom_html.element Js.t)
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

let vertical_lines_aligned_list (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list) =
  (*let _ = Printf.printf "line close_val align_num: %d %d\n"
          (line_find_closest_align_value item pos items min_distance Vertical_Left)
          (line_align_count item pos items min_distance Vertical_Left) in*)
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
  let snap_list = horizontal_lines_aligned_list item pos min_distance items in
  get_snap item pos.y min_distance snap_list

let get_item_snap_x (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list) =
  let snap_list = vertical_lines_aligned_list item pos min_distance items in
  get_snap item pos.x min_distance snap_list

let get_snap_lines
    (item : Dom_html.element Js.t)
    (pos : t)
    (items : Dom_html.element Js.t list)
    min_distance =
  let snap_list_v = vertical_lines_aligned_list item pos min_distance items in
  let snap_list_h = horizontal_lines_aligned_list item pos min_distance items in
  let rec create_lines_v_list acc = function
    | [] -> acc
    | (direction, aligns_count, distance) :: tl ->
      let acc =
        if aligns_count > 0
        then
          let line_ret =
            { is_vertical = true
            ; is_multiple = aligns_count > 1
            ; is_center = direction = Vertical_Center
            ; x =
                if direction = Vertical_Left
                then pos.x - distance
                else if direction = Vertical_Center
                then pos.x + pos.w / 2 - distance
                else if direction = Vertical_Right
                then pos.x + pos.w - distance
                else 0
            ; y = 0
            } in
          line_ret :: acc
        else acc in
      create_lines_v_list acc tl in
  let rec create_lines_h_list acc = function
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
            ; y =
                if direction = Horizontal_Top
                then pos.y - distance
                else if direction = Horizontal_Center
                then pos.y + pos.h / 2 - distance
                else if direction = Horizontal_Bottom
                then pos.y + pos.h - distance
                else 0
            } in
          line_ret :: acc
        else acc in
      create_lines_h_list acc tl in
  let list_lines_ret =
    create_lines_v_list [] snap_list_v
    @ create_lines_h_list [] snap_list_h in
  list_lines_ret

let adjust ?aspect_ratio
    ~(action : [`Resize of resize_direction | `Move])
    ~(original_position : t)
    ~(position : t)
    ~(siblings : Dom_html.element Js.t list)
    ~(parent_size : int * int)
    (item : Dom_html.element Js.t) : t * line list =
  let min_distance = 12 in
  (*Printf.printf "error: %s\n" @@ Printexc.to_string e*)
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
  (*let _ = get_item_snap_x item pos min_distance items in*)
  let position =
    { position with x = get_item_snap_x item position min_distance siblings
                  ; y = get_item_snap_y item position min_distance siblings
    }
  in

  position, get_snap_lines item position siblings min_distance
